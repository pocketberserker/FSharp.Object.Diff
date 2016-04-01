module EqualsOnlyValueProviderMethod

open FSharp.Object.Diff

type PropertyClass = {
  Prop1: string
  Prop2: string
}
with
  member this.GetProp1() = this.Prop1
  member this.GetProp2() = this.Prop2

type EncompassingClass = {
  Prop: PropertyClass
}
with
  member this.GetProp() = this.Prop

let run () =

  let prop = { Prop1 = "1"; Prop2 = "2" }
  let base_ = { Prop = prop }
  let prop2 = { Prop1 = "1"; Prop2 = "3" }
  let working = { Prop = prop2 }

  let builder = ObjectDifferBuilder.StartBuilding()

  builder.Comparison
    .OfNode(NodePath.With("Prop"))
    .ToUseEqualsMethodOfValueProvidedByMethod("GetProp1")
  |> ignore

  let node = builder.Build().Compare(working, base_)

  node.Visit(PrintingVisitor(working, base_))
