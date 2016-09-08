namespace FSharp.Object.Diff

type private MergingDifferenceVisitor<'T>(head: 'T, modified: 'T) =
  interface NodeVisitor with
    member this.Node(node, visit) =
      match node.State with
      | Removed -> node.CanonicalUnset(head)
      | Changed when node.HasChildren ->
        node.VisitChildren(this)
        visit.DontGoDeeper()
      | Added | Changed -> node.CanonicalSet(head, node.CanonicalGet(modified))
      | _ -> ()

type ObjectMerger(objectDiffer: ObjectDiffer) =

  new() = ObjectMerger(ObjectDifferBuilder.BuildDefault())

  member __.Merge(modified, base_, head) =
    let visitor = MergingDifferenceVisitor(head, modified)
    let difference = objectDiffer.Compare(modified, base_)
    difference.Visit(visitor)
    head
