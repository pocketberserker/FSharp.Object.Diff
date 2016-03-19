[<AutoOpen>]
module FSharp.Object.Diff.TypeSyntax

open System
open System.Reflection

type Type with

  static member TypesOf([<ParamArray>] values: obj[]) =
    values
    |> Array.filter ((<>) null)
    |> Array.map (fun o -> o.GetType())
    |> Array.toSeq
    |> Seq.distinct

  static member FreshInstanceOf<'T>() =
    let ctor =
      try
        typeof<'T>.GetConstructor(BindingFlags.Public ||| BindingFlags.NonPublic, null, [||], null)
        |> Some
      with _ -> None
    match ctor with
    | Some ctor -> ctor.Invoke([||])
    | None -> null
