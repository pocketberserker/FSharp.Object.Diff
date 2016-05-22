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
  let c1 = { Id = 3; Reference = None }
  let b1 = { Id = 2; Reference = Some c1 }
  let a1 = { Id = 1; Reference = Some b1 }
  let d2 = { Id = 4; Reference = None }
  let b2 = { Id = 2; Reference = Some d2 }
  let a2 = { Id = 1; Reference = Some b2 }
  let rootNode = ObjectDifferBuilder.BuildDefault().Compare(a1, a2)
  let visitor = TestablePrintingVisitor(a1, a2)
  rootNode.Visit(visitor)
  do! assertEquals ("Property at path '/Reference/Value/Reference/Value/Id' has changed from [ 4 ] to [ 3 ]" + Environment.NewLine) visitor.Output 
}

let ``prints root node if unchanged and without children`` = test {
  let visitor = TestablePrintingVisitor("foo", "foo")
  let rootNode = DiffNode.NewRootNodeWithType(typeof<string>)
  rootNode.Visit(visitor)
  do! assertEquals ("Property at path '/' has not changed" + Environment.NewLine) visitor.Output 
}
