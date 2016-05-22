namespace FSharp.Object.Diff

open System
open System.Collections
open System.Collections.Generic

type DictionaryWrapper =
  | NonGeneric of IDictionary
  | MutableGeneric of obj * Type
  | ImmutableGeneric of obj * Type

module Dictionary =

  module Generic =

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

    let contains t key o =
      let m = (makeCollectionType t).GetMethod("Contains")
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

  let (|Dictionary|_|) (o: obj) =
    match o with
    | null -> None
    | :? IDictionary as o -> Some(NonGeneric o)
    | _ ->
      match o.GetType() |> Generic.cast with
      | Some t ->
        if Generic.isReadOnly t o then Some (ImmutableGeneric(o, t))
        else Some(MutableGeneric(o, t))
      | None -> None
