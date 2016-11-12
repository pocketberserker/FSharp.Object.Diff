namespace FSharp.Object.Diff

type PrintingVisitor(working: obj, base_: obj) =

  let filter (node: DiffNode) =
    (node.IsRootNode && not node.HasChanges) || (node.HasChanges && not node.HasChildren)
  let translateState base_ modified = function
  | Ignored -> "has been ignored"
  | Changed ->
    sprintf "has changed from [ %s ] to [ %s ]" (String.toSingleLineString base_) (String.toSingleLineString modified)
  | Added ->
    String.toSingleLineString modified
    |> sprintf "has been added => [ %s ]"
  | Removed ->
    String.toSingleLineString base_
    |> sprintf "with value [ %s ] has been removed"
  | Untouched -> "has not changed"
  | Circular -> "has already been processed at another position. (Circular reference!)"
  | state -> sprintf "(%A)" state

  let differenceToString (node: DiffNode) (base_: obj) (modified: obj) =
    let nodePath = node.Path
    let stateMessage = translateState (node.CanonicalGet(base_)) (node.CanonicalGet(modified)) node.State
    let propertyMessage = sprintf "Property at path '%O' %s" nodePath stateMessage
    match node.State with
    | Circular -> propertyMessage + " (Circular reference detected: The property has already been processed at another position.)"
    | _ -> propertyMessage

  abstract member Print: string -> unit
#if PCL
  default __.Print(_) = ()
#else
  default __.Print(text) = printfn "%s" text
#endif

  abstract member Filter: DiffNode -> bool
  default __.Filter(node) = filter node

  interface NodeVisitor with
    member this.Node(node, visit) =
      if filter node then
        this.Print(differenceToString node base_ working)

#if PCL
#else
type NodeHierarchyVisitor(maxDepth: int) =
  let calculateDepth (node: DiffNode) =
    let rec inner count = function
    | null -> count
    | (parentNode: DiffNode) -> inner (count + 1) (parentNode.ParentNode)
    node.ParentNode |> inner 0
  let print (node: DiffNode) level =
    let nodeAsString = sprintf "%O ===> %O" node.Path node
    let indentedNodeString = String.indent level nodeAsString
    printfn "%s" indentedNodeString
  new() = NodeHierarchyVisitor(NodeHierarchyVisitor.Unlimited)
  static member Unlimited = 1
  interface NodeVisitor with
    member __.Node(node, visit) =
      if maxDepth = 0 then
        visit.Stop()
      let currentLevel = calculateDepth node
      if maxDepth > 0 then
        if currentLevel <= maxDepth then print node currentLevel
        else visit.DontGoDeeper()
      elif maxDepth < 0 then
        print node currentLevel
#endif
