module internal FSharp.Object.Diff.Assert

let notNull name value =
  if box value = null then invalidArg name "must not be null"
