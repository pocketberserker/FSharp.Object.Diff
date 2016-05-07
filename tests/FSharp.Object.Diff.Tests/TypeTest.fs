module FSharp.Object.Diff.Tests.TypeTest

open System
open System.Collections.Generic
open System.Runtime.Serialization
open Persimmon
open UseTestNameByReflection
open FSharp.Object.Diff

let ``mostSpecificSharedType: should return #expectedResult for #types`` = parameterize {
  source [
    ([typeof<Map<int, int>>; typeof<Map<int, int>>], Some typeof<Map<int, int>>)
    ([typeof<ISerializable>; typeof<ISerializable>], Some typeof<ISerializable>)
    ([typeof<int>; typeof<DateTime>], Some typeof<ValueType>)
    ([typeof<string>; typeof<Map<int, int>>; typeof<DateTime>], None)
  ]
  run (fun (types, expectedResult) -> test {
    do! assertEquals expectedResult (Type.mostSpecificSharedType types)
  })
}
