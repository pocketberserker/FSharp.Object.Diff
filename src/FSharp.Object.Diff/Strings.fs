module FSharp.Object.Diff.String

[<Literal>]
let LineBreakPattern = "\\s*\\n\\s*"

let toSingleLineString (o: obj) =
  if o <> null then
    o.ToString().Trim().Replace(LineBreakPattern, " \\\\ ")
  else null
