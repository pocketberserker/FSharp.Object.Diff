module FSharp.Object.Diff.String

open System.Text

[<Literal>]
let LineBreakPattern = "\\s*\\n\\s*"

let toSingleLineString (o: obj) =
  if o <> null then
    o.ToString().Trim().Replace(LineBreakPattern, " \\\\ ")
  else null

let indent times text = String.replicate times "  " + text
