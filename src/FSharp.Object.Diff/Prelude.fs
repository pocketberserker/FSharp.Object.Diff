[<AutoOpen>]
module internal FSharp.Object.Diff.Prelude

let isEqualByComparison a b =
  compare a b = 0 || compare b a = 0
