module FSharp.Object.Diff.Tests.DictionaryTest

open System.Collections.Generic
open Persimmon
open UseTestNameByReflection
open FSharp.Object.Diff
open Dictionary

let ``get dictionary wrapper`` = parameterize {
  source [
    (
      let v = Dictionary<int, string>(dict [(0, "a")])
      (box v, NonGeneric(v))
    )
    (
      let v = dict [(0, "a")]
      (box v, ImmutableGeneric(v, typeof<IDictionary<int, string>>))
    )
    (
      let v = Map.empty |> Map.add 0 "a"
      (box v, ImmutableGeneric(v, typeof<IDictionary<int, string>>))
    )
  ]
  run (fun (o, expected) -> test {
    do!
      match o with
      | Dictionary o ->
        assertEquals expected o
      | _ -> o.GetType().FullName |> sprintf "%s is not dictionary" |> fail
  })
}

module Generic =

  let ``accepts all dictionary types`` = parameterize {
    source [
      typeof<IDictionary<int, int>>
      typeof<Map<int, int>>
    ]
    run (fun t -> test {
      do! assertEquals (Some typeof<IDictionary<int, int>>) (Dictionary.Generic.cast t)
    })
  }

  let ``get value`` = parameterize {
    source [
      dict [(0, 1)] |> box
      Map.empty |> Map.add 0 1 |> box
    ]
    run (fun o -> test {
      do! assertEquals (box 1) (Dictionary.Generic.get typeof<IDictionary<int, int>> 0 o)
    })
  }

  let ``find key`` = parameterize {
    source [
      dict [(0, 1)] |> box
      Map.empty |> Map.add 0 1 |> box
    ]
    run (fun o -> test {
      do! assertPred (Dictionary.Generic.containsKey typeof<IDictionary<int, int>> 0 o)
    })
  }
