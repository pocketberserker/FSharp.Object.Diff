namespace FSharp.Object.Diff

open System
open System.Collections.Generic

type CategoryConfigurerOf =
    abstract member ToBe: [<ParamArray>] categories: string[] -> CategoryConfigurer

and CategoryConfigurer =
  abstract member OfNode: NodePath -> CategoryConfigurerOf
  abstract member OfType: Type -> CategoryConfigurerOf
  abstract member And: unit -> ObjectDifferBuilder

and CategoryService(objectDifferBuilder: ObjectDifferBuilder) =

  let nodePathCategories = NodePathValueHolder<string []>()
  let typeCategories = Dictionary<Type, string []>()

  let categoriesFromNodePathConfiguration (node: DiffNode) =
    nodePathCategories.AccumulatedValuesForNodePath(node.Path)
    |> List.fold (fun s c -> c |> Set.ofArray |> Set.union s) Set.empty

  let categoriesFromTypeConfiguration (node: DiffNode) =
    match node.Type with
    | null -> Set.empty
    | typ ->
      match typeCategories.TryGetValue(typ) with
      | true, categories -> Set.ofArray categories
      | false, _ -> Set.empty

  let categoriesFromNode (node: DiffNode) = node.Categories

  member this.OfNode(nodePath) = {
    new CategoryConfigurerOf with
      member __.ToBe([<ParamArray>] categories) =
        nodePathCategories.Put(nodePath, Some categories) |> ignore
        this :> CategoryConfigurer
  }

  member this.OfType(typ: Type) = {
    new CategoryConfigurerOf with
      member __.ToBe([<ParamArray>] categories) =
        typeCategories.Add(typ, categories)
        this :> CategoryConfigurer
  }

  member __.ResolveCategories(node) =
    Set.unionMany (seq {
      yield categoriesFromNodePathConfiguration node
      yield categoriesFromTypeConfiguration node
      yield categoriesFromNode node
    })

  interface CategoryConfigurer with
    member __.And() = objectDifferBuilder
    member this.OfNode(nodePath) = this.OfNode(nodePath)
    member this.OfType(typ) = this.OfType(typ)
  
  interface CategoryResolver with
    member this.ResolveCategories(node) = this.ResolveCategories(node)

and IntrospectionConfigurerOf =
  abstract member ToUse: Introspector -> IntrospectionConfigurer
  abstract member ToBeEnabled: unit -> IntrospectionConfigurer
  abstract member ToBeDisabled: unit -> IntrospectionConfigurer

and IntrospectionConfigurer =
  abstract member SetInstanceFactory: InstanceFactory -> IntrospectionConfigurer
  abstract member SetDefaultIntrospector: Introspector -> IntrospectionConfigurer
  abstract member HandlePropertyAccessExceptionsUsing: PropertyAccessExceptionHandler -> IntrospectionConfigurer
  abstract member OfType: Type -> IntrospectionConfigurerOf
  abstract member OfNode: NodePath -> IntrospectionConfigurerOf
  abstract member And: unit -> ObjectDifferBuilder

and IntrospectionService(objectDifferBuilder: ObjectDifferBuilder) =

  let nodePathIntrospectorHolder = NodePathValueHolder<Introspector>()
  let nodePathIntrospectionModeHolder = NodePathValueHolder<IntrospectionServiceIntrospectionMode>()
  let typeIntrospectorMap = Dictionary<Type, Introspector>()
  let typeIntrospectionModeMap = Dictionary<Type, IntrospectionServiceIntrospectionMode>()
  let mutable instanceFactory = PublicNoArgsConstructorInstanceFactory :> InstanceFactory
  let mutable defaultIntrospector: Introspector = null
  let mutable defaultPropertyAccessExceptionHandler: PropertyAccessExceptionHandler =
    DefaultPropertyAccessExceptionHandler()
    :> PropertyAccessExceptionHandler

  let isPrimitiveTypeEnumOrArray (typ: Type) =
    Type.isPrimitive typ || typ.IsArray || Type.isEnum typ

  member __.IntrospectorForNode(node: DiffNode) =
    match typeIntrospectorMap.TryGetValue(node.Type) with
    | true, introspector -> introspector
    | false, _ ->
      match nodePathIntrospectorHolder.ValueForNodePath(node.Path) with
      | Some nodePathIntrospector -> nodePathIntrospector
      | None ->
        if defaultIntrospector = null then
          defaultIntrospector <- StandardIntrospector
        defaultIntrospector

  member this.SetDefaultIntrospector(introspector) =
    Assert.notNull "The default introspector must not be null" introspector
    defaultIntrospector <- introspector
    this :> IntrospectionConfigurer

  member __.IsIntrospectable(node: DiffNode) =
    match node.Type with
    | null -> false
    | typ ->
      if isPrimitiveTypeEnumOrArray typ then false
      elif nodePathIntrospectionModeHolder.ValueForNodePath(node.Path) = Some Disabled then false
      else
        match typeIntrospectionModeMap.TryGetValue(typ) with
        | true, Disabled -> false
        | _ -> true

  member this.OfNode(path) = { new IntrospectionConfigurerOf with
    member __.ToBeDisabled() =
      nodePathIntrospectionModeHolder.Put(path, Some Disabled) |> ignore
      this :> IntrospectionConfigurer
    member __.ToBeEnabled() =
      nodePathIntrospectionModeHolder.Put(path, Some Enabled) |> ignore
      this :> IntrospectionConfigurer
    member __.ToUse(introspector) =
      nodePathIntrospectorHolder.Put(path, Some introspector) |> ignore
      this :> IntrospectionConfigurer
  }

  member this.OfType(typ) = { new IntrospectionConfigurerOf with
    member __.ToBeDisabled() =
      if typ <> null then
        if typeIntrospectionModeMap.ContainsKey(typ) then typeIntrospectionModeMap.Remove(typ) |> ignore
        typeIntrospectionModeMap.Add(typ, Disabled)
      this :> IntrospectionConfigurer
    member __.ToBeEnabled() =
      if typ <> null then
        if typeIntrospectionModeMap.ContainsKey(typ) then typeIntrospectionModeMap.Remove(typ) |> ignore
        typeIntrospectionModeMap.Add(typ, Enabled)
      this :> IntrospectionConfigurer
    member __.ToUse(introspector) =
      if typ <> null then
        if typeIntrospectorMap.ContainsKey(typ) then typeIntrospectorMap.Remove(typ) |> ignore
        typeIntrospectorMap.Add(typ, introspector)
      this :> IntrospectionConfigurer
  }

  member this.TypeInfoForNode(node: DiffNode) =
    let beanType = node.Type
    let introspector = this.IntrospectorForNode(node)
    let typeInfo = introspector.Introspect(beanType)
    typeInfo.InstanceFactory <- instanceFactory
    typeInfo

  interface IntrospectionConfigurer with
    member __.And() = objectDifferBuilder
    member this.OfNode(path) = this.OfNode(path)
    member this.OfType(typ) = this.OfType(typ)
    member this.SetDefaultIntrospector(introspector) = this.SetDefaultIntrospector(introspector)
    member this.SetInstanceFactory(factory) =
      instanceFactory <- InstanceFactoryFallbackDecorator(factory)
      this :> IntrospectionConfigurer
    member this.HandlePropertyAccessExceptionsUsing(exceptionHandler) =
      defaultPropertyAccessExceptionHandler <- exceptionHandler
      this :> IntrospectionConfigurer

  interface IsIntrospectableResolver with
    member this.IsIntrospectable(node) = this.IsIntrospectable(node)

  interface TypeInfoResolver with
    member this.TypeInfoForNode(node) = this.TypeInfoForNode(node)

  interface PropertyAccessExceptionHandlerResolver with
    member __.ResolvePropertyAccessExceptionHandler(_, _) = defaultPropertyAccessExceptionHandler

and InclusionConfigurerToInclude =
  abstract member Category: string -> InclusionConfigurerToInclude
  abstract member Type: Type -> InclusionConfigurerToInclude
  abstract member Node: NodePath -> InclusionConfigurerToInclude
  abstract member PropertyName: string -> InclusionConfigurerToInclude
  abstract member PropertyNameOfType: Type * string [] -> InclusionConfigurerToInclude
  abstract member Also: unit -> InclusionConfigurer
  abstract member And: unit -> ObjectDifferBuilder

and InclusionConfigurerToExclude =
  abstract member Category: string -> InclusionConfigurerToExclude
  abstract member Type: Type -> InclusionConfigurerToExclude
  abstract member Node: NodePath -> InclusionConfigurerToExclude
  abstract member PropertyName: string -> InclusionConfigurerToExclude
  abstract member PropertyNameOfType: Type * string [] -> InclusionConfigurerToExclude
  abstract member Also: unit -> InclusionConfigurer
  abstract member And: unit -> ObjectDifferBuilder

and InclusionConfigurer =
  abstract member Include: unit -> InclusionConfigurerToInclude
  abstract member Exclude: unit -> InclusionConfigurerToExclude
  abstract member ResolveUsing: InclusionResolver -> InclusionConfigurer
  abstract member And: unit -> ObjectDifferBuilder

and InclusionService(categoryResolver: CategoryResolver, rootConfiguration: ObjectDifferBuilder) =

  let inclusionResolvers = ResizeArray<InclusionResolver>()
  let mutable categoryInclusionResolver: CategoryInclusionResolver option = None
  let mutable typeInclusionResolver: TypeInclusionResolver option = None
  let mutable nodePathInclusionResolver: NodePathInclusionResolver option = None
  let mutable propertyNameInclusionResolver: PropertyNameInclusionResolver option = None
  let mutable typePropertyConfigInclusionResolver: TypePropertyConfigInclusionResolver option = None

//  do
//    inclusionResolvers.Add(TypePropertyAnnotationInclusionResolver())

  let getInclusion (node: DiffNode) (inclusionResolver: InclusionResolver) =
    inclusionResolver.GetInclusion(node)

  let isIgnored node =
    let rec inner strictIncludeModeEnabled isExplicitlyIncluded = function
    | [] -> strictIncludeModeEnabled && not isExplicitlyIncluded
    | (x: InclusionResolver)::xs ->
      let strictIncludeModeEnabled = if x.EnablesStrictIncludeMode() then true else strictIncludeModeEnabled
      match getInclusion node x with
      | Excluded -> true
      | Included -> inner strictIncludeModeEnabled true xs
      | Default -> inner strictIncludeModeEnabled isExplicitlyIncluded xs
    inner false false (List.ofSeq inclusionResolvers)

  let newCategoryInclusionResolver () = CategoryInclusionResolver(categoryResolver)

  let setCategoryInclusion inclusion category =
    match categoryInclusionResolver with
    | None ->
      let resolver = newCategoryInclusionResolver ()
      categoryInclusionResolver <- Some resolver
      inclusionResolvers.Add(resolver)
    | Some _ -> ()
    categoryInclusionResolver |> Option.iter (fun x -> x.SetInclusion(category, inclusion))

  let setTypeInclusion inclusion typ =
    match typeInclusionResolver with
    | None ->
      let resolver = TypeInclusionResolver()
      typeInclusionResolver <- Some resolver
      inclusionResolvers.Add(resolver)
    | Some _ -> ()
    typeInclusionResolver |> Option.iter (fun x -> x.SetInclusion(typ, inclusion))

  let setNodePathInclusion inclusion nodePath =
    match nodePathInclusionResolver with
    | None ->
      let resolver = NodePathInclusionResolver()
      nodePathInclusionResolver <- Some resolver
      inclusionResolvers.Add(resolver)
    | Some _ -> ()
    nodePathInclusionResolver |> Option.iter (fun x -> x.SetInclusion(nodePath, inclusion))

  let setPropertyNameInclusion inclusion propertyName =
    match propertyNameInclusionResolver with
    | None ->
      let resolver = PropertyNameInclusionResolver()
      propertyNameInclusionResolver <- Some resolver
      inclusionResolvers.Add(resolver)
    | Some _ -> ()
    propertyNameInclusionResolver |> Option.iter (fun x -> x.SetInclusion(propertyName, inclusion))

  let setPropertyNameOfTypeInclusion inclusion typ propertyNames =
    for propertyName in propertyNames do
      match typePropertyConfigInclusionResolver with
      | None ->
        let resolver = TypePropertyConfigInclusionResolver()
        typePropertyConfigInclusionResolver <- Some resolver
        inclusionResolvers.Add(resolver)
      | Some _ -> ()
      typePropertyConfigInclusionResolver |> Option.iter (fun x -> x.SetInclusion(typ, propertyName, inclusion))

  interface InclusionConfigurer with

    member __.And() = rootConfiguration

    member this.Exclude() =
      { new InclusionConfigurerToExclude with
        member __.Also() = this :> InclusionConfigurer
        member __.And() = rootConfiguration
        member this.Category(category) =
          setCategoryInclusion Excluded category
          this
        member this.Node(nodePath) =
          setNodePathInclusion Excluded nodePath
          this
        member this.PropertyName(propertyName) =
          setPropertyNameInclusion Excluded propertyName
          this
        member this.PropertyNameOfType(typ, [<ParamArray>] propertyNames) =
          setPropertyNameOfTypeInclusion Excluded typ propertyNames
          this
        member this.Type(typ) =
          setTypeInclusion Excluded typ
          this
      }

    member this.Include() =
      { new InclusionConfigurerToInclude with
        member __.Also() = this :> InclusionConfigurer
        member __.And() = rootConfiguration
        member this.Category(category) =
          setCategoryInclusion Included category
          this
        member this.Node(nodePath) =
          setNodePathInclusion Included nodePath
          this
        member this.PropertyName(propertyName) =
          setPropertyNameInclusion Included propertyName
          this
        member this.PropertyNameOfType(typ, [<ParamArray>] propertyNames) =
          setPropertyNameOfTypeInclusion Included typ propertyNames
          this
        member this.Type(typ) =
          setTypeInclusion Included typ
          this
      }

    member this.ResolveUsing(inclusionResolver) =
      inclusionResolvers.Add(inclusionResolver)
      this :> InclusionConfigurer

  interface IsIgnoredResolver with
    member __.IsIgnored(node) =
      if node.IsRootNode then false
      else isIgnored node

and ComparisonConfigurerOf =
  abstract member ToUse: ComparisonStrategy -> ComparisonConfigurer
  abstract member ToUseEqualsMethod: unit -> ComparisonConfigurer
  abstract member ToUseEqualsMethodOfValueProvidedByMethod: string -> ComparisonConfigurer
  abstract member ToUseCompareToMethod: unit -> ComparisonConfigurer

and [<AbstractClass>] ComparisonConfigurerAbstractOf() =
  abstract member ToUse: ComparisonStrategy -> ComparisonConfigurer
  interface ComparisonConfigurerOf with
    member this.ToUse(comparisonStrategy) = this.ToUse(comparisonStrategy)
    member this.ToUseEqualsMethod() = this.ToUse(EqualsOnlyComparisonStrategy())
    member this.ToUseEqualsMethodOfValueProvidedByMethod(propertyName) = this.ToUse(EqualsOnlyComparisonStrategy(propertyName))
    member this.ToUseCompareToMethod() = this.ToUse(ComparableComparisonStrategy)

and ComparisonConfigurerOfType(comparisonConfigurer: ComparisonConfigurer, typ: Type, typeComparisonStrategyMap: Dictionary<Type, ComparisonStrategy>) =
  inherit ComparisonConfigurerAbstractOf()
  override __.ToUse(comparisonStrategy) =
    typeComparisonStrategyMap.Add(typ, comparisonStrategy)
    comparisonConfigurer

and ComparisonConfigurerOfNodePath(comparisonConfigurer: ComparisonConfigurer, nodePath: NodePath, nodePathComparisonStrategies: NodePathValueHolder<ComparisonStrategy>) =
  inherit ComparisonConfigurerAbstractOf()
  override __.ToUse(comparisonStrategy) =
    nodePathComparisonStrategies.Put(nodePath, Some comparisonStrategy) |> ignore
    comparisonConfigurer

and ComparisonConfigurerOfPrimitiveTypes =
  abstract member ToTreatDefaultValuesAs: PrimitiveDefaultValueMode -> ComparisonConfigurer

and private ComparisonConfigurerOfPrimitiveTypesImpl(mode: ref<PrimitiveDefaultValueMode>, this: ComparisonService) =
  interface ComparisonConfigurerOfPrimitiveTypes with
    member __.ToTreatDefaultValuesAs(m) =
      mode := m
      this :> ComparisonConfigurer

and ComparisonConfigurer =
  abstract member OfNode: NodePath -> ComparisonConfigurerOf
  abstract member OfType: Type -> ComparisonConfigurerOf
  abstract member OfPrimitiveTypes: unit -> ComparisonConfigurerOfPrimitiveTypes
  abstract member And: unit -> ObjectDifferBuilder

and ComparisonService(objectDifferBuilder: ObjectDifferBuilder) =

  static let equalsOnlyComparisonStrategy = EqualsOnlyComparisonStrategy()
  let nodePathComparisonStrategies = NodePathValueHolder<ComparisonStrategy>()
  let typeComparisonStrategyMap = Dictionary<Type, ComparisonStrategy>()
  let primitiveDefaultValueMode =  ref UnAssigned

  member __.ResolveComparisonStrategy(node: DiffNode) =
    let valueType = node.Type
    match nodePathComparisonStrategies.ValueForNodePath(node.Path) with
    | Some comparisonStrategy -> comparisonStrategy
    | None ->
      match typeComparisonStrategyMap.TryGetValue(valueType) with
      | true, v -> v
      | false, _ ->
        if Type.isSimple valueType then
          if Type.isComparable valueType then ComparableComparisonStrategy :> ComparisonStrategy
          else equalsOnlyComparisonStrategy :> ComparisonStrategy
        else
          let comparisonStrategyResolver = ObjectDiffPropertyComparisonStrategyResolver
          let objectDiffProperty = node.GetPropertyAttribute<ObjectDiffPropertyAttribute>()
          match comparisonStrategyResolver.ComparisonStrategyForAttribute(objectDiffProperty) with
          | Some v -> v :> ComparisonStrategy
          | None when valueType <> null ->
            let objectDiffEqualsOnlyType =
              let xs = Type.getCustomAttributes<ObjectDiffEqualsOnlyAttribute> false valueType
              if Array.isEmpty xs then null
              else xs.[0]
            match comparisonStrategyResolver.ComparisonStrategyForAttribute(objectDiffEqualsOnlyType) with
            | Some v -> v :> ComparisonStrategy
            | None -> null
          | None -> null

  interface ComparisonConfigurer with
    member __.And() = objectDifferBuilder
    member this.OfNode(nodePath) =
      ComparisonConfigurerOfNodePath(this, nodePath, nodePathComparisonStrategies)
      :> ComparisonConfigurerOf
    member this.OfPrimitiveTypes() =
      ComparisonConfigurerOfPrimitiveTypesImpl(primitiveDefaultValueMode, this)
      :> ComparisonConfigurerOfPrimitiveTypes
    member this.OfType(typ) =
      ComparisonConfigurerOfType(this, typ, typeComparisonStrategyMap)
      :> ComparisonConfigurerOf
  
  interface ComparisonStrategyResolver with
    member this.ResolveComparisonStrategy(node) = this.ResolveComparisonStrategy(node)
  
  interface PrimitiveDefaultValueModeResolver with
    member __.ResolvePrimitiveDefaultValueMode(_) = !primitiveDefaultValueMode

and IdentityConfigurerOfCollectionItems =
  abstract member Via: IdentityStrategy -> IdentityConfigurer

and IdentityConfigurer =
  abstract member OfCollectionItems: NodePath -> IdentityConfigurerOfCollectionItems
  abstract member OfCollectionItems: Type * string -> IdentityConfigurerOfCollectionItems
  abstract member And: unit -> ObjectDifferBuilder

and private CollectionItemIdentityServiceOfCollectionItemsByNodePath(
                                                                     nodePath: NodePath,
                                                                     nodePathIdentityStrategies: ValueNode<IdentityStrategy>,
                                                                     identityConfigure: IdentityConfigurer
  ) =

  interface IdentityConfigurerOfCollectionItems with
    member __.Via(identityStrategy) =
      nodePathIdentityStrategies.GetNodeForPath(nodePath).Value <- Some identityStrategy
      identityConfigure

and private CollectionItemIdentityServiceOfCollectionItemsByTypeProperty(
                                                                         typ: Type,
                                                                         propertyName: string,
                                                                         typePropertyIdentityStrategyResolver: TypePropertyIdentityStrategyResolver,
                                                                         identityConfigure: IdentityConfigurer
  ) =

  interface IdentityConfigurerOfCollectionItems with
    member __.Via(identityStrategy) =
      typePropertyIdentityStrategyResolver.SetStrategy(identityStrategy, typ, propertyName)
      identityConfigure

and CollectionItemIdentityService(identityConfigurer: IdentityConfigurer) =
 
  let nodePathIdentityStrategies = ValueNode<IdentityStrategy>()
  let typePropertyIdentityStrategyResolver = TypePropertyIdentityStrategyResolver()

  member __.ResolveIdentityStrategy(node: DiffNode) =
    match typePropertyIdentityStrategyResolver.Resolve(node) with
    | null ->
      match nodePathIdentityStrategies.GetNodeForPath(node.Path).Value with
      | Some s -> s
      | None -> EqualsIdentityStrategy :> IdentityStrategy
    | s -> s

  member __.OfCollectionItems(nodePath: NodePath) =
    CollectionItemIdentityServiceOfCollectionItemsByNodePath(nodePath, nodePathIdentityStrategies, identityConfigurer)
    :> IdentityConfigurerOfCollectionItems

  member __.OfCollectionItems(typ: Type, propertyName: string) =
    CollectionItemIdentityServiceOfCollectionItemsByTypeProperty(typ, propertyName, typePropertyIdentityStrategyResolver, identityConfigurer)
    :> IdentityConfigurerOfCollectionItems

  interface IdentityStrategyResolver with
    member this.ResolveIdentityStrategy(node) = this.ResolveIdentityStrategy(node)

and IdentityService(objectDifferBuilder: ObjectDifferBuilder) as this =

  let collectionItemIdentityService = CollectionItemIdentityService(this)

  member __.And() = objectDifferBuilder
  member __.OfCollectionItems(nodePath) =
    collectionItemIdentityService.OfCollectionItems(nodePath)
  member __.OfCollectionItems(typ, propertyName) =
    collectionItemIdentityService.OfCollectionItems(typ, propertyName)
  member __. ResolveIdentityStrategy(node) =
    collectionItemIdentityService.ResolveIdentityStrategy(node)

  interface IdentityConfigurer with
    member this.And() = this.And()
    member this.OfCollectionItems(nodePath) = this.OfCollectionItems(nodePath)
    member this.OfCollectionItems(typ, propertyName) = this.OfCollectionItems(typ, propertyName)
  
  interface IdentityStrategyResolver with
    member this. ResolveIdentityStrategy(node) = this.ResolveIdentityStrategy(node)

and FilteringConfigurer =
  abstract member ReturnNodesWithState: DiffNodeState * bool -> FilteringConfigurer
  abstract member ReturnNodesWithState: DiffNodeState -> FilteringConfigurer
  abstract member OmitNodesWithState: DiffNodeState -> FilteringConfigurer
  abstract member And: unit -> ObjectDifferBuilder

and ReturnableNodeService( objectDifferBuilder: ObjectDifferBuilder) =

  let stateFilterSettings = Dictionary<DiffNodeState, bool>()

  do
    stateFilterSettings.Add(Ignored, false)
    stateFilterSettings.Add(Inaccessible, false)
    stateFilterSettings.Add(Untouched, false)
    stateFilterSettings.Add(Circular, true)
    stateFilterSettings.Add(Added, true)
    stateFilterSettings.Add(Removed, true)
    stateFilterSettings.Add(Changed, true)

  member this.ReturnNodesWithState(state, enabled) =
    stateFilterSettings.Remove(state) |> ignore
    stateFilterSettings.Add(state, enabled)
    this :> FilteringConfigurer

  interface FilteringConfigurer with
    member __.And() = objectDifferBuilder
    member this.OmitNodesWithState(state) = this.ReturnNodesWithState(state, false)
    member this.ReturnNodesWithState(state, enabled) = this.ReturnNodesWithState(state, enabled)
    member this.ReturnNodesWithState(state) = this.ReturnNodesWithState(state, true)
    
  
  interface IsReturnableResolver with
    member __.IsReturnable(node) =
      if node.IsRootNode then true
      elif node.State = Untouched && node.HasChildren then true
      else stateFilterSettings.[node.State]

and CircularReferenceConfigurer =
  abstract member MatchCircularReferencesUsing: CircularReferenceMatchingMode -> CircularReferenceConfigurer
  abstract member HandleCircularReferenceExceptionsUsing: CircularReferenceExceptionHandler -> CircularReferenceConfigurer
  abstract member And: unit -> ObjectDifferBuilder

and CircularReferenceService(objectDifferBuilder: ObjectDifferBuilder) =

  let mutable circularReferenceMatchingMode = CircularReferenceMatchingMode.EqualityOperator
  let mutable circularReferenceExceptionHandler = Some { new CircularReferenceExceptionHandler with
    member __.OnCircularReferenceException(node) =
      // TODO: logging
//      let message = "Detected circular reference in node at path {}. "
//        + "Going deeper would cause an infinite loop, so I'll stop looking at "
//        + "this instance along the current path."
      ()
  }

  interface CircularReferenceConfigurer with
    member this.MatchCircularReferencesUsing(matchingMode) =
      circularReferenceMatchingMode <- matchingMode
      this :> CircularReferenceConfigurer
    member this.HandleCircularReferenceExceptionsUsing(exceptionHandler) =
      circularReferenceExceptionHandler <- Some exceptionHandler
      this :> CircularReferenceConfigurer
    member __.And() = objectDifferBuilder
  
  interface CircularReferenceDetectorFactory with
    member __.CreateCircularReferenceDetector() =
      match circularReferenceMatchingMode with
      | CircularReferenceMatchingMode.ReferenceEqualMethod -> CircularReferenceDetector(ReferenceEqualMethod)
      | CircularReferenceMatchingMode.EqualityOperator -> CircularReferenceDetector(EqualityOperator)

  interface CircularReferenceExceptionHandler with
    member __.OnCircularReferenceException(node) =
      circularReferenceExceptionHandler
      |> Option.iter (fun x -> x.OnCircularReferenceException(node))

and DifferConfigurer =
  abstract member Register: DifferFactory -> ObjectDifferBuilder

and DifferService(objectDifferBuilder: ObjectDifferBuilder) =

  let differFactories = ResizeArray<DifferFactory>()

  member __.DifferFactories = differFactories :> seq<DifferFactory>

  interface DifferConfigurer with
    member __.Register(differFactory) =
      differFactories.Add(differFactory)
      objectDifferBuilder

and ObjectDifferBuilder() as this =
  
  let categoryService = CategoryService(this)
  let introspectionService = IntrospectionService(this)
  let inclusionService = InclusionService(categoryService, this)
  let comparisonService = ComparisonService(this)
  let identityService = IdentityService(this)
  let returnableNodeService = ReturnableNodeService(this)
  let circularReferenceService = CircularReferenceService(this)
  let differService = DifferService(this)

  let nodeQueryService =
    DefaultNodeQueryService(categoryService, introspectionService, inclusionService, returnableNodeService, comparisonService, comparisonService)

  let newDifferDispatcher differProvider =
    DifferDispatcher(differProvider, circularReferenceService, circularReferenceService, inclusionService, returnableNodeService, introspectionService, categoryService)

  let newBeanDiffer differDispatcher =
    BeanDiffer(differDispatcher, introspectionService, returnableNodeService, comparisonService, introspectionService)

  let newPrimitiveDiffer () =
    PrimitiveDiffer(comparisonService)

  let newCollectionDiffer differDispatcher =
    CollectionDiffer(differDispatcher, comparisonService, identityService)

  let newMapDiffer differDispatcher =
    DictionaryDiffer(differDispatcher, comparisonService)

  let createCustomDiffers differDispatcher =
    let differs = ResizeArray<Differ>()
    differService.DifferFactories
    |> Seq.iter (fun differFactory ->
      differs.Add(differFactory.CreateDiffer(differDispatcher, nodeQueryService))
    )
    differs

  static member StartBuilding() = ObjectDifferBuilder()

  static member BuildDefault() = ObjectDifferBuilder.StartBuilding().Build()

  member __.Filtering = returnableNodeService :> FilteringConfigurer
  member __.Introspection = introspectionService :> IntrospectionConfigurer
  member __.CircularReferenceHandling = circularReferenceService :> CircularReferenceConfigurer
  member __.Inclusion = inclusionService :> InclusionConfigurer
  member __.Comparison = comparisonService :> ComparisonConfigurer
  member __.Identity = identityService
  member __.Categories = categoryService :> CategoryConfigurer
  member __.Differs = differService :> DifferConfigurer

  member __.Build() =
    let differProvider = DifferProvider()
    let differDispatcher = newDifferDispatcher(differProvider)
    differProvider.Push(newBeanDiffer differDispatcher)
    differProvider.Push(newCollectionDiffer differDispatcher)
    differProvider.Push(newMapDiffer differDispatcher)
    differProvider.Push(newPrimitiveDiffer ())
    differProvider.PushAll(createCustomDiffers differDispatcher)
    ObjectDiffer(differDispatcher)
