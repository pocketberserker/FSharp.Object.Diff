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
      ("\naaa\nbbb\nccc\n", @"aaa \\ bbb \\ ccc") // head newline and tail new line are trimed before replacing
      ("aaa\n\nbbb", @"aaa \\ bbb") // \s eats \n (This behavior may be bug from original project)
    ]
    run test
  }