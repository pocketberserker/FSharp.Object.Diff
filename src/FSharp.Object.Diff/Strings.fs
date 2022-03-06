module FSharp.Object.Diff.String

open System.Text.RegularExpressions

[<Literal>]
let LineBreakPattern = @"\s*\n\s*"

let toSingleLineString (o: obj) =
  match o with
  | null -> null
  | notNull ->
      Regex.Replace(notNull.ToString().Trim(), LineBreakPattern, @" \\ ")

let indent times text = String.replicate times "  " + text
