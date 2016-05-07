namespace FSharp.Object.Diff

open System
open System.Collections.Generic
open System.Threading

[<AllowNullLiteral>]
type Differ =
  abstract member Accepts: Type -> bool
  abstract member Compare: DiffNode * Instances -> DiffNode

[<Sealed>]
type PrimitiveDiffer(primitiveDefaultValueModeResolver: PrimitiveDefaultValueModeResolver) =
  let shouldTreatPrimitiveDefaultsAsUnassigned node =
    primitiveDefaultValueModeResolver.ResolvePrimitiveDefaultValueMode(node) = UnAssigned
  member __.Accepts(typ: Type) = Type.isPrimitive typ
  interface Differ with
    member this.Accepts(typ: Type) = this.Accepts(typ)
    member this.Compare(parentNode, instances) =
      if not <| this.Accepts(instances.Type) then
        raise <| ArgumentException("The primitive differ can only deal with primitive types.")
      let node = DiffNode(parentNode, instances.SourceAccessor, instances.Type)
      if shouldTreatPrimitiveDefaultsAsUnassigned node && instances.HasBeenAdded then
        node.State <- Added
      elif shouldTreatPrimitiveDefaultsAsUnassigned node && instances.HasBeenAdded then
        node.State <- Removed
      elif not <| instances.AreEqual then
        node.State <- Changed
      node

type DifferProvider() =
  let differs = new ResizeArray<Differ>()
  member __.Push(differ) = differs.Add(differ)
  member __.PushAll(ds) = differs.AddRange(ds)
  member __.RetrieveDifferForType(typ: Type) =
    if typ = null then raise <| ArgumentException("Missing 'type'")
    match differs |> Seq.tryFind (fun d -> d.Accepts(typ)) with
    | Some differ -> differ
    | None -> raise <| InvalidOperationException("Couldn't find a differ for type: " + typ.FullName)

type DifferDispatcher(
                      differProvider: DifferProvider,
                      circularReferenceDetectorFactory: CircularReferenceDetectorFactory,
                      circularReferenceExceptionHandler: CircularReferenceExceptionHandler,
                      isIgnoredResolver: IsIgnoredResolver,
                      isReturnableResolver: IsReturnableResolver,
                      propertyAccessExceptionHandlerResolver: PropertyAccessExceptionHandlerResolver,
                      categoryResolver: CategoryResolver
  ) =

  [<ThreadStatic; DefaultValue>]
  static val mutable private workingThreadLocal : CircularReferenceDetector
  [<ThreadStatic; DefaultValue>]
  static val mutable private baseThreadLocal : CircularReferenceDetector

  let resetInstanceMemory () =
    DifferDispatcher.workingThreadLocal <- circularReferenceDetectorFactory.CreateCircularReferenceDetector()
    DifferDispatcher.baseThreadLocal <- circularReferenceDetectorFactory.CreateCircularReferenceDetector()

  let clearInstanceMemory () =
    DifferDispatcher.workingThreadLocal <- null
    DifferDispatcher.baseThreadLocal <- null

  do
    resetInstanceMemory ()

  let compare parentNode (instances: Instances) =
    let differ = differProvider.RetrieveDifferForType(instances.Type)
    if differ = null then
      instances.Type.FullName
      |> sprintf "Couldn't create Differ for type '%s'. This mustn't happen, as there should always be a fallback differ."
      |> InvalidOperationException
      |> raise
    differ.Compare(parentNode, instances)

  let rec findNodeMatchingPropertyPath (node: DiffNode) nodePath =
    if node = null then null
    elif node.Matches(nodePath) then node
    else findNodeMatchingPropertyPath node.ParentNode nodePath

  let newCircularNode parentNode (instances: Instances) circleStartPath =
    let node = DiffNode(parentNode, instances.SourceAccessor, instances.Type)
    node.State <- Circular
    node.CircleStartPath <- circleStartPath
    node.CircleStartNode <- findNodeMatchingPropertyPath parentNode circleStartPath
    node

  let getNodePath (parentNode: DiffNode) (instances: Instances) =
    if parentNode = null then NodePath.WithRoot()
    else
      NodePath.StartBuildingFrom(parentNode.Path).Element(instances.SourceAccessor.ElementSelector).Build()

  let transactionalPushToCircularReferenceDetectors nodePath (instances: Instances) =
    DifferDispatcher.workingThreadLocal.Push(instances.Working, nodePath)
    try
      DifferDispatcher.baseThreadLocal.Push(instances.Base, nodePath)
    with
     :? CircularReferenceException ->
       DifferDispatcher.workingThreadLocal.Remove(instances.Working)
       reraise ()

  let rememberInstances parentNode instances =
    let nodePath = getNodePath parentNode instances
    transactionalPushToCircularReferenceDetectors nodePath instances

  let forgetInstances parentNode (instances: Instances) =
    let nodePath = getNodePath parentNode instances
    DifferDispatcher.workingThreadLocal.Remove(instances.Working)
    DifferDispatcher.baseThreadLocal.Remove(instances.Base)

  let compareWithCircularReferenceTracking parentNode instances =
    let node =
      try
        rememberInstances parentNode instances
        let node: ref<DiffNode> = ref null
        try
          node := compare parentNode instances
        finally
          if !node <> null then forgetInstances parentNode instances
        !node
      with
        CircularReferenceException(nodePath) ->
          let node = newCircularNode parentNode instances nodePath
          circularReferenceExceptionHandler.OnCircularReferenceException(node)
          node
    if parentNode <> null then resetInstanceMemory ()
    node 

  let compareWithAccessor parentNode (parentInstances: Instances) (accessor: Accessor) =
    let node = new DiffNode(parentNode, accessor, null)
    if isIgnoredResolver.IsIgnored(node) then
      node.State <- Ignored
      node
    else
      let result, accessedInstances =
        match accessor with
        | :? PropertyAwareAccessor as accessor ->
          try
            (None, parentInstances.Access(accessor))
          with
            :? PropertyReadException as e ->
              node.State <- Inaccessible
              let parentType = parentInstances.Type
              let propertyName = accessor.PropertyName
              let exceptionHandler = propertyAccessExceptionHandlerResolver.ResolvePropertyAccessExceptionHandler(parentType, propertyName)
              if exceptionHandler <> null then
                exceptionHandler.OnPropertyReadException(e, node)
              (Some node, parentInstances)
        | _ -> (None, parentInstances.Access(accessor))
      match result with
      | Some node -> node
      | None when accessedInstances.AreNull ->
        DiffNode(parentNode, accessedInstances.SourceAccessor, accessedInstances.Type)
      | None ->
        compareWithCircularReferenceTracking parentNode accessedInstances

  member __.Dispatch(parentNode, parentInstances, accessor) =
    let node = compareWithAccessor parentNode parentInstances accessor
    if parentNode <> null && isReturnableResolver.IsReturnable(node) then
      parentNode.AddChild(node)
    if node <> null then
      node.AddCategories(categoryResolver.ResolveCategories(node) |> Set.toList)
    node

  member __.ResetInstanceMemory() = resetInstanceMemory ()
  member __.ClearInstanceMemory() = clearInstanceMemory ()

[<Sealed>]
type BeanDiffer(
                differDispatcher: DifferDispatcher,
                isIntrospectableResolver: IsIntrospectableResolver,
                isReturnableResolver: IsReturnableResolver,
                comparisonStrategyResolver: ComparisonStrategyResolver,
                typeInfoResolver: TypeInfoResolver
  ) =

  let compareUsingIntrospection (beanNode: DiffNode) (beanInstances: Instances) =
    let typeInfo = typeInfoResolver.TypeInfoForNode(beanNode)
    beanNode.TypeInfo <- typeInfo
    for propertyAccessor in typeInfo.Accessors do
      let propertyNode = differDispatcher.Dispatch(beanNode, beanInstances, propertyAccessor)
      if isReturnableResolver.IsReturnable(propertyNode) then
        beanNode.AddChild(propertyNode)

  let compareUsingAppropriateMethod (beanNode: DiffNode) (instances: Instances) =
    let comparisonStrategy = comparisonStrategyResolver.ResolveComparisonStrategy(beanNode)
    if comparisonStrategy <> null then
     comparisonStrategy.Compare(beanNode, instances.Type, instances.Working, instances.Base)
    elif isIntrospectableResolver.IsIntrospectable(beanNode) then
      compareUsingIntrospection beanNode instances

  member __.Accepts(typ: Type) = (not <| typ.IsPrimitive) && (not <| typ.IsArray)

  member __.Compare(parentNode, instances: Instances) =
    let beanNode = DiffNode(parentNode, instances.SourceAccessor, instances.Type)
    if instances.AreNull || instances.AreSame then
      beanNode.State <- Untouched
    elif instances.HasBeenAdded then
      compareUsingAppropriateMethod beanNode instances
      beanNode.State <- Added
    elif instances.HasBeenRemoved then
      compareUsingAppropriateMethod beanNode instances
      beanNode.State <- Removed
    else compareUsingAppropriateMethod beanNode instances
    beanNode

  interface Differ with

    member this.Accepts(typ: Type) = this.Accepts(typ)

    member this.Compare(parentNode, instances) = this.Compare(parentNode, instances)

[<Sealed>]
type CollectionDiffer(
                      differDispatcher: DifferDispatcher,
                      comparisonStrategyResolver: ComparisonStrategyResolver,
                      identityStrategyResolver: IdentityStrategyResolver) =

  let compareUsingComparisonStrategy (collectionNode: DiffNode) (collectionInstances: Instances) (comparisonStrategy: ComparisonStrategy) =
    comparisonStrategy.Compare(
      collectionNode,
      collectionInstances.Type,
      (match collectionInstances.TryGetWorking<System.Collections.IEnumerable>() with | Some v -> v | None -> null),
      (match collectionInstances.TryGetBase<System.Collections.IEnumerable>() with | Some v -> v | None -> null)
      )

  let contains (haystack: System.Collections.IEnumerable) needle (identityStrategy: IdentityStrategy) =
    let rec inner result (e: System.Collections.IEnumerator) =
      if result then result
      elif e.MoveNext() then 
        if identityStrategy.Equals(needle, e.Current) then inner true e
        else inner result e
      else false
    inner false (haystack.GetEnumerator())

  let remove (from: ResizeArray<obj>) these identityStrategy =
    for item in from do
      if contains these item identityStrategy then
        from.Remove(item) |> ignore

  let compareItems (collectionNode: DiffNode) (collectionInstances: Instances) items identityStrategy =
    for item in items do
      let itemAccessor = CollectionItemAccessor(item, identityStrategy)
      differDispatcher.Dispatch(collectionNode, collectionInstances, itemAccessor)
      |> ignore

  let getOrEmpty (xs: System.Collections.IEnumerable option) =
    match xs with
    | Some xs ->
      let cs = ResizeArray()
      for x in xs do cs.Add(x)
      cs :> obj seq
    | None -> Seq.empty

  let compareInternally (collectionNode: DiffNode) (collectionInstances: Instances) identityStrategy =
    let working = collectionInstances.TryGetWorking<System.Collections.IEnumerable>() |> getOrEmpty
    let base_ = collectionInstances.TryGetBase<System.Collections.IEnumerable>() |> getOrEmpty

    let added = ResizeArray(working)
    let removed = ResizeArray(working)
    let known = ResizeArray(working)

    remove added base_ identityStrategy
    remove removed working identityStrategy
    remove known added identityStrategy
    remove known removed identityStrategy

    compareItems collectionNode collectionInstances added identityStrategy
    compareItems collectionNode collectionInstances removed identityStrategy
    compareItems collectionNode collectionInstances known identityStrategy

  let newNode parentNode (collectionInstances: Instances) =
    DiffNode(parentNode, collectionInstances.SourceAccessor, collectionInstances.Type)

  member __.Accepts(typ: Type)  =
    typeof<System.Collections.IEnumerable>.IsAssignableFrom(typ)

  member this.Compare(parentNode, instances) =
    let collectionNode = newNode parentNode instances
    let identityStrategy = identityStrategyResolver.ResolveIdentityStrategy(collectionNode)
    if identityStrategy <> null then
      collectionNode.ChildIdentityStrategy <- identityStrategy
    if instances.HasBeenAdded then
      let addedItems = instances.TryGetWorking<System.Collections.IEnumerable>() |> getOrEmpty
      compareItems collectionNode instances addedItems identityStrategy
      collectionNode.State <- Added
    elif instances.HasBeenRemoved then
      let removedItems = instances.TryGetBase<System.Collections.IEnumerable>() |> getOrEmpty
      compareItems collectionNode instances removedItems identityStrategy
      collectionNode.State <- Removed
    elif instances.AreSame then
      collectionNode.State <- Untouched
    else
      let comparisonStrategy = comparisonStrategyResolver.ResolveComparisonStrategy(collectionNode)
      if comparisonStrategy = null then
        compareInternally collectionNode instances identityStrategy
      else compareUsingComparisonStrategy collectionNode instances comparisonStrategy
    collectionNode

  interface Differ with

    member this.Accepts(typ: Type) = this.Accepts(typ)

    member this.Compare(parentNode, instances) = this.Compare(parentNode, instances)

[<Sealed>]
type MapDiffer(differDispatcher: DifferDispatcher, comparisonStrategyResolver: ComparisonStrategyResolver) =

  let findAddedKeys(instances: Instances) =
    let source = instances.TryGetWorking<System.Collections.IDictionary>()
    let filter = instances.TryGetBase<System.Collections.IDictionary>()
    let xs = ResizeArray()
    source |> Option.iter (fun s -> for k in s.Keys do xs.Add(k))
    filter |> Option.iter (fun s -> for k in s.Keys do xs.Remove(k) |> ignore)
    xs :> obj seq

  let findRemovedKeys(instances: Instances) =
    let source = instances.TryGetBase<System.Collections.IDictionary>()
    let filter = instances.TryGetWorking<System.Collections.IDictionary>()
    let xs = ResizeArray()
    source |> Option.iter (fun s -> for k in s.Keys do xs.Add(k))
    filter |> Option.iter (fun s -> for k in s.Keys do xs.Remove(k) |> ignore)
    xs :> obj seq

  let findKnownKeys(instances: Instances) =
    match instances.TryGetBase<System.Collections.IDictionary>() with
    | Some v ->
      let xs = ResizeArray()
      for k in v.Keys do
        if not <| xs.Contains(k) then xs.Add(k)
      xs
      |> Seq.filter (fun x -> not (findAddedKeys instances |> Seq.exists ((=) x) || findRemovedKeys instances |> Seq.exists ((=) x)))
    | None -> Seq.empty
    
  let compareEntries (mapNode: DiffNode) mapInstances keys =
    for key in keys do
      differDispatcher.Dispatch(mapNode, mapInstances, MapEntryAccessor(key)) |> ignore

  member __.Compare(parentNode, instances: Instances) =
    let mapNode = DiffNode(parentNode, instances.SourceAccessor, instances.Type)
    if instances.HasBeenAdded then
      match instances.TryGetWorking<System.Collections.IDictionary>() with
      | Some v ->
        let xs = ResizeArray()
        for k in v.Keys do xs.Add(k)
        xs :> obj seq
      | None -> Seq.empty
      |> compareEntries mapNode instances
      mapNode.State <- Added
    elif instances.HasBeenRemoved then
      match instances.TryGetBase<System.Collections.IDictionary>() with
      | Some v ->
        let xs = ResizeArray()
        for k in v.Keys do xs.Add(k)
        xs :> obj seq
      | None -> Seq.empty
      |> compareEntries mapNode instances
      mapNode.State <- Removed
    elif instances.AreSame then
      mapNode.State <- Untouched
    elif comparisonStrategyResolver.ResolveComparisonStrategy(mapNode) <> null then
      comparisonStrategyResolver.ResolveComparisonStrategy(mapNode)
        .Compare(
          mapNode,
          instances.Type,
          (match instances.TryGetWorking<System.Collections.IDictionary>() with | Some v -> v | None -> null),
          match instances.TryGetBase<System.Collections.IDictionary>() with | Some v -> v | None -> null)
    else
      findAddedKeys instances |> compareEntries mapNode instances
      findRemovedKeys instances |> compareEntries mapNode instances
      findKnownKeys instances |> compareEntries mapNode instances
    mapNode

  member __.Accepts(typ: Type)  =
    if typ <> null then typeof<System.Collections.IDictionary>.IsAssignableFrom(typ)
    else false
  
  interface Differ with

    member this.Accepts(typ: Type) = this.Accepts(typ)

    member this.Compare(parentNode, instances) = this.Compare(parentNode, instances)
