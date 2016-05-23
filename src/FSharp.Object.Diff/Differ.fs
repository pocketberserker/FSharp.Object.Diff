namespace FSharp.Object.Diff

open System
open System.Collections
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
  member this.Compare(parentNode: DiffNode, instances: Instances) =
    if not <| this.Accepts(instances.Type) then
      raise <| ArgumentException("The primitive differ can only deal with primitive types.")
    let node = DiffNode(parentNode, instances.SourceAccessor, instances.Type)
    if shouldTreatPrimitiveDefaultsAsUnassigned node && instances.HasBeenAdded then
      node.State <- Added
    elif shouldTreatPrimitiveDefaultsAsUnassigned node && instances.HasBeenRemoved then
      node.State <- Removed
    elif not <| instances.AreEqual then
      node.State <- Changed
    node
  interface Differ with
    member this.Accepts(typ: Type) = this.Accepts(typ)
    member this.Compare(parentNode, instances) = this.Compare(parentNode, instances)

type DifferProvider() =

  let differs = new ResizeArray<Differ>()

  member __.Push(differ) = differs.Insert(0, differ)
  member __.PushAll(ds) = differs.AddRange(ds)

  abstract member RetrieveDifferForType: Type -> Differ
  default __.RetrieveDifferForType(typ) =
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

  member __.Dispatch(parentNode: DiffNode, parentInstances, accessor) =
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
      (
        match collectionInstances.Working with
        | IsCollection(NonGenericCollection v) -> box v
        | IsCollection(ImmutableGenericCollection(v, _) | MutableGenericCollection(v, _) | FSharpList(v, _)) -> v
        | _ -> null
      ),
      match collectionInstances.Base with
      | IsCollection(NonGenericCollection v) -> box v
      | IsCollection(ImmutableGenericCollection(v, _) | MutableGenericCollection(v, _) | FSharpList(v, _)) -> v
      | _ -> null
    )

  let contains (haystack: IEnumerable) needle (identityStrategy: IdentityStrategy) =
    let rec inner (e: IEnumerator) =
      if e.MoveNext() then 
        if identityStrategy.Equals(needle, e.Current) then true
        else inner e
      else false
    inner (haystack.GetEnumerator())

  let remove (from: ResizeArray<obj>) these identityStrategy =
    from.RemoveAll(fun item -> contains these item identityStrategy)
    |> ignore

  let compareItems (collectionNode: DiffNode) (collectionInstances: Instances) (items: IEnumerable) identityStrategy =
    let rec inner index (e: IEnumerator) =
      if e.MoveNext() then
        let itemAccessor = CollectionItemAccessor(e.Current, Some index, identityStrategy)
        differDispatcher.Dispatch(collectionNode, collectionInstances, itemAccessor)
        |> ignore
        inner (index + 1) e
      else ()
    items.GetEnumerator() |> inner 0

  let getOrEmpty (o: obj) =
    match o with
    | IsCollection(NonGenericCollection xs) ->
      Some(xs :> IEnumerable)
    | IsCollection(MutableGenericCollection(xs, _) | ImmutableGenericCollection(xs, _) | FSharpList(xs, _)) ->
      Some(xs :?> IEnumerable)
    | _ -> None
    |> Option.map (fun xs ->
      let cs = ResizeArray()
      for x in xs do cs.Add(x)
      cs :> obj seq
    )
    |> function
    | Some v -> v
    | None -> Seq.empty

  let compareInternally (collectionNode: DiffNode) (collectionInstances: Instances) identityStrategy =
    let working = collectionInstances.Working |> getOrEmpty
    let base_ = collectionInstances.Base |> getOrEmpty

    let added = ResizeArray(working)
    let removed = ResizeArray(base_)
    let known = ResizeArray(base_)

    remove added base_ identityStrategy
    remove removed working identityStrategy
    remove known added identityStrategy
    remove known removed identityStrategy

    compareItems collectionNode collectionInstances added identityStrategy
    compareItems collectionNode collectionInstances removed identityStrategy
    compareItems collectionNode collectionInstances known identityStrategy

  let newNode parentNode (collectionInstances: Instances) =
    DiffNode(parentNode, collectionInstances.SourceAccessor, collectionInstances.Type)

  member __.Accepts(typ: Type) =
    if typ <> null then
      if typeof<IList>.IsAssignableFrom(typ) then true
      else
        match Collection.ICollection.cast typ with
        | Some _ -> true
        | None ->
          match Collection.FSharpList.cast typ with
          | Some  _ -> true
          | None -> false
    else false

  member this.Compare(parentNode, instances) =
    let collectionNode = newNode parentNode instances
    let identityStrategy = identityStrategyResolver.ResolveIdentityStrategy(collectionNode)
    if identityStrategy <> null then
      collectionNode.ChildIdentityStrategy <- identityStrategy
    if instances.HasBeenAdded then
      let addedItems = instances.Working |> getOrEmpty
      compareItems collectionNode instances addedItems identityStrategy
      collectionNode.State <- Added
    elif instances.HasBeenRemoved then
      let removedItems = instances.Base |> getOrEmpty
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
type DictionaryDiffer(differDispatcher: DifferDispatcher, comparisonStrategyResolver: ComparisonStrategyResolver) =

  let findKeys source filter =
    let source =
      match source with
      | IsDictionary d -> Some d
      | _ -> None
    let filter =
      match filter with
      | IsDictionary d -> Some d
      | _ -> None
    let xs = ResizeArray()
    source
    |> Option.iter (function
    | NonGenericDictionary s -> for k in s.Keys do xs.Add(k)
    | MutableGenericDictionary(o, t) | ImmutableGenericDictionary(o, t) ->
      for k in Dictionary.IDictionary.keys t o do xs.Add(k)
    )
    filter
    |> Option.iter (function
    | NonGenericDictionary s -> for k in s.Keys do xs.Remove(k) |> ignore
    | MutableGenericDictionary(o, t) | ImmutableGenericDictionary(o, t) ->
      for k in Dictionary.IDictionary.keys t o do xs.Remove(k) |> ignore
    )
    xs :> obj seq

  let findAddedKeys (instances: Instances) = findKeys instances.Working instances.Base

  let findRemovedKeys (instances: Instances) = findKeys instances.Base instances.Working

  let findKnownKeys(instances: Instances) =
    match instances.Base with
    | IsDictionary(NonGenericDictionary d) -> d.Keys :> IEnumerable |> Some
    | IsDictionary(MutableGenericDictionary(o, t) | ImmutableGenericDictionary(o, t)) -> Dictionary.IDictionary.keys t o |> Some
    | _ -> None
    |> function
    | Some keys ->
      let xs = ResizeArray()
      for k in keys do
        if not <| xs.Contains(k) then xs.Add(k)
      xs
      |> Seq.filter (fun x -> not (findAddedKeys instances |> Seq.exists ((=) x) || findRemovedKeys instances |> Seq.exists ((=) x)))
    | None -> Seq.empty
    
  let compareEntries (dictNode: DiffNode) dictInstances keys =
    for key in keys do
      differDispatcher.Dispatch(dictNode, dictInstances, DictionaryEntryAccessor(key)) |> ignore

  member __.Compare(parentNode, instances: Instances) =
    let dictNode = DiffNode(parentNode, instances.SourceAccessor, instances.Type)
    if instances.HasBeenAdded then
      match instances.Working with
      | IsDictionary(NonGenericDictionary v) ->
        let xs = ResizeArray()
        for k in v.Keys do xs.Add(k)
        xs :> obj seq
      | IsDictionary(MutableGenericDictionary(v, t) | ImmutableGenericDictionary(v, t)) ->
        let xs = ResizeArray()
        for k in Dictionary.IDictionary.keys t v do xs.Add(k)
        xs :> obj seq
      | _ -> Seq.empty
      |> compareEntries dictNode instances
      dictNode.State <- Added
    elif instances.HasBeenRemoved then
      match instances.Base with
      | IsDictionary(NonGenericDictionary v) ->
        let xs = ResizeArray()
        for k in v.Keys do xs.Add(k)
        xs :> obj seq
      | IsDictionary(MutableGenericDictionary(v, t) | ImmutableGenericDictionary(v, t)) ->
        let xs = ResizeArray()
        for k in Dictionary.IDictionary.keys t v do xs.Add(k)
        xs :> obj seq
      | _ -> Seq.empty
      |> compareEntries dictNode instances
      dictNode.State <- Removed
    elif instances.AreSame then
      dictNode.State <- Untouched
    elif comparisonStrategyResolver.ResolveComparisonStrategy(dictNode) <> null then
      comparisonStrategyResolver.ResolveComparisonStrategy(dictNode)
        .Compare(
          dictNode,
          instances.Type,
          (
            match instances.Working with
            | IsDictionary(NonGenericDictionary v) -> box v
            | IsDictionary(ImmutableGenericDictionary(v, _) | MutableGenericDictionary(v, _)) ->
            v | _ -> null
          ),
          match instances.Base with
          | IsDictionary(NonGenericDictionary v) -> box v
          | IsDictionary(ImmutableGenericDictionary(v, _) | MutableGenericDictionary(v, _)) -> v
          | _ -> null
        )
    else
      findAddedKeys instances |> compareEntries dictNode instances
      findRemovedKeys instances |> compareEntries dictNode instances
      findKnownKeys instances |> compareEntries dictNode instances
    dictNode

  member __.Accepts(typ: Type)  =
    if typ <> null then
      if typeof<IDictionary>.IsAssignableFrom(typ) then true
      else
        match Dictionary.IDictionary.cast typ with
        | Some _ -> true
        | None -> false
    else false
  
  interface Differ with

    member this.Accepts(typ: Type) = this.Accepts(typ)

    member this.Compare(parentNode, instances) = this.Compare(parentNode, instances)
