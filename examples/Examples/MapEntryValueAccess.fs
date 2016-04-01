module MapEntryValueAccess

open System.Collections.Generic
open FSharp.Object.Diff

let run () =
  let base_ = Dictionary<int, string>()
  let working = Dictionary<int, string>()
  working.Add(4, "Locke")
  working.Add(8, "Reyes")
  working.Add(15, "Ford")
  working.Add(16, "Jarrah")
  working.Add(23, "Shephard")
  working.Add(42, "Kwon")

  let mapNode = ObjectDifferBuilder.BuildDefault().Compare(working, base_)
  mapNode.VisitChildren({ new NodeVisitor with
    member __.Node(node, visit) =
      let key = (node.ElementSelector :?> MapKeyElementSelector).Key
      let value = node.CanonicalGet(working)
      printfn "%A => %A" key value
  })
