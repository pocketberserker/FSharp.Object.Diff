module FSharp.Object.Diff.Tests.ComparisonServiceTest

open System
open Persimmon
open UseTestNameByReflection
open FSharp.Object.Diff

let ``resolveComparisonStrategy: returns ComparableComparisonStrategy for simple types implementing Comparable`` = parameterize {
  source [
    typeof<bool>
    typeof<char>
    typeof<float32>
    typeof<float>
    typeof<sbyte>
    typeof<uint16>
    typeof<uint32>
    typeof<uint64>
    typeof<byte>
    typeof<int16>
    typeof<int>
    typeof<int64>
    typeof<DateTime>
    typeof<TimeSpan>
  ]
  run (fun typ -> test {
    let service = ObjectDifferBuilder().Comparison :?> ComparisonService
    let node = DiffNode(null, RootAccessor, typ)
    do!
      assertPred (service.ResolveComparisonStrategy(node) :? ComparableComparisonStrategy)
  })
}
