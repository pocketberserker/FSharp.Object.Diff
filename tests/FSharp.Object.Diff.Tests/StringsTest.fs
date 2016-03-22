module StringsTest

open Persimmon
open UseTestNameByReflection

open FSharp.Object.Diff

let ``toSingleLineString`` () =
  let test (input, expected) = test {
    let res = String.toSingleLineString input
    do! assertEquals expected res
  }
  parameterize {
    source [
      (null, null)
      ("", "")
      (" aaa ", "aaa")
      (" aaa\nbbb ", @"aaa \\ bbb")
      (" aaa \n bbb ", @"aaa \\ bbb")
    ]
    run test
  }