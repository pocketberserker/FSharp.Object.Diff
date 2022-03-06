module FSharp.Object.Diff.Tests.EqualsIdentityStrategyTest

open Persimmon
open UseTestNameByReflection
open FSharp.Object.Diff

let equal = parameterize {
  source [
    (null, null, true)
    (null, "foo", false)
    ("foo", "bar", false)
    ("foo", "foo", true)
  ]
  run (fun (a, b, equal) -> test {
    do! assertEquals equal (EqualsIdentityStrategy.IsEqual(a, b))
  })
}
