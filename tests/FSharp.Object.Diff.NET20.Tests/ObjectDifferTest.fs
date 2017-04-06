module FSharp.Object.Diff.Tests.ObjectDifferTest

open Persimmon
open UseTestNameByReflection
open FSharp.Object.Diff

let ``compare with different types`` = test {
  do!
    ObjectDifferBuilder.BuildDefault().Compare(box "foo", box 1).State
    |> assertEquals Changed
}
