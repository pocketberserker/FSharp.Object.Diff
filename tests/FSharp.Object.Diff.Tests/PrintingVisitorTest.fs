module FSharp.Object.Diff.Tests.PrintingVisitorTest

open System
open System.Text
open Persimmon
open UseTestNameByReflection
open FSharp.Object.Diff

type TestablePrintingVisitor(working: obj, base_: obj) =
  inherit PrintingVisitor(working, base_)
  let sb = StringBuilder()
  override __.Print(text) = sb.AppendLine(text) |> ignore
  member __.Output = sb.ToString()

let ``omits intermediate nodes with changed child nodes`` = test {
  let c1 = { Id = "c"; Reference = None }
  let b1 = { Id = "b"; Reference = Some c1 }
  let a1 = { Id = "a"; Reference = Some b1 }
  let d2 = { Id = "d"; Reference = None }
  let b2 = { Id = "b"; Reference = Some d2 }
  let a2 = { Id = "a"; Reference = Some b2 }
  let rootNode = ObjectDifferBuilder.BuildDefault().Compare(a1, a2)
  let visitor = TestablePrintingVisitor(a1, a2)
  rootNode.Visit(visitor)
  do! assertEquals ("Property at path '/Reference/Value/Reference/Value/Id' has changed from [ d ] to [ c ]" + Environment.NewLine) visitor.Output 
}

let ``prints root node if unchanged and without children`` = test {
  let visitor = TestablePrintingVisitor("foo", "foo")
  let rootNode = DiffNode.NewRootNodeWithType(typeof<string>)
  rootNode.Visit(visitor)
  do! assertEquals ("Property at path '/' has not changed" + Environment.NewLine) visitor.Output 
}

let ``avoid infinite loop`` = test {
  let c1 = RecursionPropertyObject("c", None)
  let b1 = RecursionPropertyObject("b", Some c1)
  c1.Parent <- Some b1
  let a1 = RecursionPropertyObject("a", Some b1)
  b1.Parent <- Some a1
  let d2 = RecursionPropertyObject("d", None)
  let b2 = RecursionPropertyObject("b", Some d2)
  d2.Parent <- Some b2
  let a2 = RecursionPropertyObject("a", Some b2)
  b2.Parent <- Some a2
  let rootNode = ObjectDifferBuilder.BuildDefault().Compare(a1, a2)
  let visitor = TestablePrintingVisitor(a1, a2)
  rootNode.Visit(visitor)
  do! assertEquals ("Property at path '/Reference/Value/Reference/Value/Id' has changed from [ d ] to [ c ]" + Environment.NewLine) visitor.Output 
}
