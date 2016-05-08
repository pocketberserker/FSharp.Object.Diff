namespace FSharp.Object.Diff

open System
open System.Collections.Generic
open System.Threading
open System.Text
open Printf

type DiffNodeState =
  | Added
  | Changed
  | Removed
  | Untouched
  | Circular
  | Ignored
  | Inaccessible
with
  member this.Reason =
    match this with
    | Added -> "The value has been added to the working object"
    | Changed -> "The value exists but differs between the base and working object"
    | Removed -> "The value has been removed from the working object"
    | Untouched -> "The value is identical in the working and base object"
    | Circular -> "Special state to mark circular references"
    | Ignored -> "The value has not been looked at and has been ignored"
    | Inaccessible -> "When a comparison was not possible because the underlying value was not accessible"

type VisitState =
  | Continue
  | ContinueButDoNotGoDeeper
  | Stopped

type NodeVisit() =
  let mutable state = Continue
  member __.State = state
  member __.Stop() = state <- Stopped
  member __.DontGoDeeper() = state <- ContinueButDoNotGoDeeper
  member __.IsAllowedToGoDeeper = state = Continue

exception StopVisitationException

type NodeVisitor =
  abstract member Node: DiffNode * NodeVisit -> unit

and [<AllowNullLiteral>] DiffNode(parentNode: DiffNode, accessor: Accessor, valueType: Type) =
  let children = Dictionary<ElementSelector, DiffNode>()
  let mutable parentNode = parentNode
  let mutable valueType = valueType
  let mutable childIdentityStrategy: IdentityStrategy = null
  let mutable additionalCategories = Set.empty<string>

  new(parentNode, accessor) = DiffNode(parentNode, accessor, null)
  new() = DiffNode(DiffNode.Root, RootAccessor)

  static member Root: DiffNode = null

  member val CircleStartPath: NodePath = null with get, set
  member val CircleStartNode: DiffNode = null with get, set

  member __.AddCategories(categories) =
    additionalCategories <- List.foldBack Set.add categories additionalCategories 

  member val State = Untouched with get, set

  member __.ChildIdentityStrategy with private get() = childIdentityStrategy and set(value) = childIdentityStrategy <- value

  member val TypeInfo: TypeInfo = null with get, set

  member this.Type
    with get() =
      if valueType <> null then valueType
      elif this.TypeInfo <> null then this.TypeInfo.Type
      elif accessor :? TypeAwareAccessor then (accessor :?> TypeAwareAccessor).Type
      else null
    and set(value) = valueType <- value

  static member NewRootNode() = DiffNode()
  static member NewRootNodeWithType(typ: Type) =
    let rootNode = DiffNode.NewRootNode()
    rootNode.Type <- typ
    rootNode

  member __.Path =
    if parentNode <> null then
      NodePath.StartBuildingFrom(parentNode.Path)
        .Element(accessor.ElementSelector)
        .Build()
    elif accessor :? RootAccessor then NodePath.WithRoot()
    else NodePath.StartBuilding().Element(accessor.ElementSelector).Build()

  member this.Matches(path: NodePath) = path.Matches(this.Path)

  member __.ElementSelector = accessor.ElementSelector

  member __.HasChildren = children.Count <> 0
  member __.ChildCount = children.Count

  member __.IsRootNode = accessor :? RootAccessor

  member __.Child(elementSelector: ElementSelector) =
    match elementSelector with
    | :? CollectionItemElementSelector as elementSelector when childIdentityStrategy <> null ->
      match children.TryGetValue(elementSelector.WithIdentityStrategy(childIdentityStrategy)) with
      | true, v -> v
      | false, _ -> null
    | _ ->
      match children.TryGetValue(elementSelector) with
      | true, v -> v
      | false, _ -> null
  member this.Child(propertyName) = this.Child(BeanPropertyElementSelector(propertyName))
  member this.Child(elementSelectors: ElementSelector list) =
    match elementSelectors with
    | [] -> null
    | [selector] when selector = RootElementSelector.Instance ->
      if this.IsRootNode then this else null
    | [selector] -> this.Child(selector)
    | selector :: xs ->
      let child =
        if selector = RootElementSelector.Instance then
          if this.IsRootNode then this else null
        else this.Child(selector)
      if child <> null then child.Child(xs)
      else null
  member this.Child(nodePath: NodePath) =
    if parentNode <> null then parentNode.Child(nodePath.ElementSelectors)
    else this.Child(nodePath.ElementSelectors)

  member __.ParentNode
    with get() = parentNode
    and internal set(value) =
      if parentNode <> null && parentNode <> value then
        raise <| InvalidOperationException("The parent of a node cannot be changed, once it's set.")
      else parentNode <- value

  member this.Visit(visitor: NodeVisitor, visit: NodeVisit) =
    try
      visitor.Node(this, visit)
    with :? StopVisitationException ->
      visit.Stop()
    if visit.IsAllowedToGoDeeper && this.HasChildren then
      this.VisitChildren(visitor)
    if visit.State = Stopped then raise StopVisitationException
  member this.Visit(visitor) =
    let visit = NodeVisit()
    try
      this.Visit(visitor, visit)
    with :? StopVisitationException -> ()

  member __.VisitChildren(visitor: NodeVisitor) =
    for child in children.Values do
      try
        child.Visit(visitor)
      with :? StopVisitationException -> ()

  member this.HasChanges =
    match this.State with
    | Added | Changed | Removed -> true
    | _ ->
      let result = ref<obj> false
      this.VisitChildren({ new NodeVisitor with
        member __.Node(node, visit) =
          if node.HasChanges then
            Interlocked.Exchange(result, true) |> ignore
            visit.Stop()
      })
      !result |> unbox<bool>

  member this.AddChild(node: DiffNode) =
    if Object.ReferenceEquals(node, this) then
      raise <| ArgumentException("Detected attempt to add a node to itself. This would cause inifite loops and must never happen.")
    elif node.IsRootNode then
      raise <| ArgumentException("Detected attempt to add root node as child. This is not allowed and must be a mistake.")
    elif node.ParentNode <> null && not <| Object.ReferenceEquals(node.ParentNode, this) then
      raise <| ArgumentException("Detected attempt to add child node that is already the child of another node. Adding nodes multiple times is not allowed, since it could cause infinite loops.")
    if node.ParentNode = null then
      node.ParentNode <- this
    if children.ContainsKey(node.ElementSelector) then children.Remove(node.ElementSelector) |> ignore
    children.Add(node.ElementSelector, node)
    if this.State = Untouched && node.HasChanges then
      this.State <- Changed

  member __.VisitParents(visitor: NodeVisitor) =
    let visit = NodeVisit()
    if parentNode <> null then
      visitor.Node(parentNode, visit)
      if visit.State <> Stopped then
        parentNode.VisitParents(visitor)

  member __.PropertyName =
    match accessor with
    | :? PropertyAwareAccessor as accessor -> accessor.PropertyName
    | _ when parentNode <> null -> parentNode.PropertyName
    | _ -> null

  member private this.NewInstance() =
    if this.TypeInfo <> null then this.TypeInfo.NewInstance()
    else null

  member __.Get(target: obj) = accessor.Get(target)
  member __.Set(target: obj, value: obj) = accessor.Set(target, value)
  member __.Unset(target: obj) = accessor.Unset(target)
  member this.CanonicalGet(target) =
    let target =
      if parentNode <> null then parentNode.CanonicalGet(target)
      else target
    this.Get(target)
  member this.CanonicalSet(target, value) =
    let target =
      if parentNode <> null then
        let parent = parentNode.CanonicalGet(target)
        let parent =
          if parent = null then
            let parent = parentNode.NewInstance()
            parentNode.CanonicalSet(target, parent)
            parent
          else parent
        parent
      else target
    this.Set(target, value)
  member this.CanonicalUnset(target) =
    let target =
      if parentNode <> null then parentNode.CanonicalGet(target)
      else target
    this.Unset(target)

  member __.Categories =
    let categories =
      if parentNode <> null then parentNode.Categories
      else Set.empty
    match accessor with
    | :? CategoryAware as accessor ->
      accessor.CategoriesFromAttribute
      |> Set.union categories
    | _ -> categories
    |> Set.union additionalCategories

  override this.ToString() =
    let sb = StringBuilder()
    bprintf sb "%s(state=%A" (this.GetType().Name) this.State 
    if this.Type <> null then
      bprintf sb ", type=%s" this.Type.FullName
    if this.ChildCount = 1 then
      bprintf sb ", %d child" this.ChildCount
    elif this.ChildCount > 1 then
      bprintf sb ", %d children" this.ChildCount
    else
      bprintf sb ", no children"
    if not <| Set.isEmpty this.Categories then
      bprintf sb ", categorized as %A" this.Categories
    bprintf sb ", accessed via %O)" accessor
    sb.ToString()

  member __.IsProperyAware = accessor :? PropertyAwareAccessor

  member __.GetPropertyAttribute<'T when 'T :> Attribute and 'T : null>() =
    match accessor with
    | :? PropertyAwareAccessor as accessor -> accessor.GetPropertyAttribute<'T>()
    | _ -> null

  member __.PropertyAttributes =
    match accessor with
    | :? PropertyAwareAccessor as accessor -> accessor.PropertyAttributes
    | _ -> Seq.empty

  member private __.Accessor = accessor

  override this.Equals(other) =
    match other with
    | null -> false
    | _ when Object.ReferenceEquals(this, other) -> true
    | :? DiffNode as other -> accessor = other.Accessor
    | _ -> false

  override __.GetHashCode() = hash accessor

type IdentityStrategyResolver =
  abstract member ResolveIdentityStrategy: DiffNode -> IdentityStrategy
