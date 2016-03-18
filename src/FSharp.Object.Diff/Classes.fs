[<AutoOpen>]
module FSharp.Object.Diff.TypeSyntax

open System

type Type with
  static member TypesOf([<ParamArray>] values: obj[]) =
    values
    |> Array.filter ((<>) null)
    |> Array.map (fun o -> o.GetType())
    |> Array.toSeq
    |> Seq.distinct

