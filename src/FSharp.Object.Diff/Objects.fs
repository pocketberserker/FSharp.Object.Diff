[<AutoOpen>]
module FSharp.Object.Diff.ObjectSyntax

open System

type Object with
  static member IsEqual(a: obj, b: obj) =
    if a <> null then a.Equals(b)
    elif b <> null then b.Equals(a)
    else true
