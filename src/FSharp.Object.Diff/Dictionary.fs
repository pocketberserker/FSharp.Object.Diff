namespace FSharp.Object.Diff

open System
open System.Collections
open System.Collections.Generic

type DictionaryWrapper =
  | NonGenericDictionary of IDictionary
  | MutableGenericDictionary of obj * Type
  | ImmutableGenericDictionary of obj * Type

module Dictionary =

  module IDictionary =

    let get (t: Type) key o =
      let p = t.GetProperty("Item")
      p.GetValue(o, [|key|])

    let private collection = typedefof<ICollection<_>>
    let private kvp = typedefof<KeyValuePair<_, _>>

    let private makeCollectionType (t: Type) =
      collection.MakeGenericType(kvp.MakeGenericType(t.GetGenericArguments()))

    let clear t o =
      let m = (makeCollectionType t).GetMethod("Clear")
      m.Invoke(o, [||])
      |> unbox<unit>

    let remove t key o =
      let m = (makeCollectionType t).GetMethod("Remove")
      m.Invoke(o, [|key|])
      |> unbox<bool>

    let add t key value o =
      let m = (makeCollectionType t).GetMethod("Add")
      m.Invoke(o, [|key, value|])
      |> unbox<unit>

    let containsKey (t: Type) key o =
      let m = t.GetMethod("ContainsKey")
      m.Invoke(o, [|key|])
      |> unbox<bool>

    let isReadOnly t o =
      let p = (makeCollectionType t).GetProperty("IsReadOnly")
      p.GetValue(o, [||])
      |> unbox<bool>

    let keys (t: Type) o =
      let p = t.GetProperty("Keys")
      p.GetValue(o, [||])
      :?> IEnumerable

    let private idict = typedefof<IDictionary<_, _>>

    let cast (t: Type) =
      if t = null then None
      elif t.IsGenericType then
        let ps = t.GetGenericArguments()
        if Array.length ps = 2 then
          let idict = idict.MakeGenericType(ps)
          if idict.IsAssignableFrom(t) then Some idict
          else None
        else None
      else None


  let tryFindAllAssignable xs =
    xs 
    |> Seq.map IDictionary.cast
    |> Seq.reduce (fun acc t ->
      match (acc, t) with
      | Some acc, Some t ->
        if acc.IsAssignableFrom(t) then Some acc
        elif t.IsAssignableFrom(acc) then Some t
        else None
      | _ -> None)

[<AutoOpen>]
module DictionarySyntax =

  open Dictionary

  let (|IsDictionary|_|) (o: obj) =
    match o with
    | null -> None
    | :? IDictionary as o -> Some(NonGenericDictionary o)
    | _ ->
      match o.GetType() |> IDictionary.cast with
      | Some t ->
        if IDictionary.isReadOnly t o then Some (ImmutableGenericDictionary(o, t))
        else Some(MutableGenericDictionary(o, t))
      | None -> None
