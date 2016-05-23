module FSharp.Object.Diff.Tests.CollectionTest

open System.Collections.Generic
open Persimmon
open UseTestNameByReflection
open FSharp.Object.Diff

let ``get collection wrapper`` = parameterize {
  source [
    (
      let v = ResizeArray<int>()
      (box v, NonGenericCollection(v))
    )
    (
      let v = [0]
      (box v, FSharpList(v, typeof<int list>))
    )
    (
      let v = [| 0 |]
      (box v, NonGenericCollection v)
    )
  ]
  run (fun (o, expected) -> test {
    do!
      match o with
      | IsCollection o ->
        assertEquals expected o
      | _ -> o.GetType().FullName |> sprintf "%s is not collection" |> fail
  })
}

module ICollection =

  let ``accepts all collection types`` = parameterize {
    source [
      typeof<ResizeArray<int>>
    ]
    run (fun t -> test {
      do! assertEquals (Some typeof<ICollection<int>>) (Collection.ICollection.cast t)
    })
  }

module IList =

  let ``get value`` = parameterize {
    source [
      (
        let v = ResizeArray<int>()
        v.Add(1)
        box v
      )
    ]
    run (fun o -> test {
      do! assertEquals (box 1) (Collection.IList.item typeof<IList<int>> 0 o)
    })
  }

module FSharpList =

  let ``accepts type`` = test {
    do! assertEquals (Some typeof<int list>) (Collection.FSharpList.cast typeof<int list>)
  }

  let ``get value`` = test {
    do! assertEquals (box 1) (Collection.FSharpList.item typeof<int list> 0 [1])
  }

  let ``get length`` = test {
    do! assertEquals (box 1) (Collection.FSharpList.length typeof<int list> [0])
  }
