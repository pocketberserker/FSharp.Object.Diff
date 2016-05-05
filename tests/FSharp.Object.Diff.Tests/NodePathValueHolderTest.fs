module  FSharp.Object.Diff.Tests.NodePathValueHolderTest

open Persimmon
open UseTestNameByReflection
open FSharp.Object.Diff

let ``should store value at node path`` = parameterize {
  source [
    (NodePath.WithRoot(), "foo1")
    (NodePath.With("a"), "foo2")
    (NodePath.With("a", "b"), "foo3")
  ]
  run (fun (nodePath, value) -> test {
    let valueHolder = NodePathValueHolder<string>()
    valueHolder.Put(nodePath, Some value) |> ignore
    do! assertEquals (Some value) (valueHolder.ValueForNodePath(nodePath))
  })
}

let ``should return null for unknown paths`` = test {
  let valueHolder = NodePathValueHolder<string>()
  do! valueHolder.ValueForNodePath(NodePath.With("a"))
    |> assertEquals None
}

let ``should return accumulated values along path`` = test {
  let valueHolder = NodePathValueHolder<string>()
  valueHolder.Put(NodePath.WithRoot(), Some "foo1") |> ignore
  valueHolder.Put(NodePath.With("a"), Some "foo2") |> ignore
  valueHolder.Put(NodePath.With("a", "b"), Some "foo3") |> ignore
  do! valueHolder.AccumulatedValuesForNodePath(NodePath.With("a", "b"))
    |> assertEquals ["foo1"; "foo2"; "foo3"]
}
