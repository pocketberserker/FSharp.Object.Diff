namespace FSharp.Object.Diff

open System
open System.Collections.Generic

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
      elif not <| instances.AreEqual() then
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

  // TODO: implement
  // workingThreadLocal
  // baseThreadLocal

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

  let compareWithCircularReferenceTracking parentNode instances =
    let node =
      try
        //rememberInstances parentNode instances
        let node: ref<DiffNode> = ref null
        try
          node := compare parentNode instances
        finally
          //if !node <> null then forgetInstances parentNode instances
          ()
        !node
      with
        CircularReferenceException(nodePath) ->
          let node = newCircularNode parentNode instances nodePath
          circularReferenceExceptionHandler.OnCircularReferenceException(node)
          node
    //if parentNode <> null then resetInstanceMemory ()
    node 

  let compareWithAccessor parentNode (parentInstances: Instances) (accessor: Accessor) =
    let node = new DiffNode(parentNode, accessor, null)
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
    | None when accessedInstances.AreNull() ->
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

  interface Differ with

    member this.Accepts(typ: Type) = this.Accepts(typ)

    member this.Compare(parentNode, instances) =
      let beanNode = DiffNode(parentNode, instances.SourceAccessor, instances.Type)
      if instances.AreNull() || instances.AreSame() then
        beanNode.State <- Untouched
      elif instances.HasBeenAdded then
        compareUsingAppropriateMethod beanNode instances
        beanNode.State <- Added
      elif instances.HasBeenAdded then
        compareUsingAppropriateMethod beanNode instances
        beanNode.State <- Removed
      else compareUsingAppropriateMethod beanNode instances
      beanNode
