module Ignore

open FSharp.Object.Diff

type Person = {
  Name: string
  Password: string
}

let run () =
  let b = { Name = "foo"; Password = "1234" }
  let w = { Name = "foo"; Password = "9876" }
  let builder = ObjectDifferBuilder.StartBuilding()
  builder.Inclusion.Exclude().Node(NodePath.With("Password")) |> ignore
  let node = builder.Build().Compare(w, b)
  node.Visit(PrintingVisitor(w, b))
