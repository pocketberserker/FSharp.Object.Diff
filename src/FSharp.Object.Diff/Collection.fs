namespace FSharp.Object.Diff

open System
open System.Collections
open System.Collections.Generic

type CollectionWrapper =
  | NonGenericCollection of IList
  | MutableGenericCollection of obj * Type
  | ImmutableGenericCollection of obj * Type
  | FSharpList of obj * Type

module Collection =

  [<RequireQualifiedAccess>]
  module ICollection =

    let private collection = typedefof<ICollection<_>>

    let clear t o =
      let m = Type.getMethod "Clear" t
      m.Invoke(o, [||])
      |> unbox<unit>

    let remove t item o =
      let m = Type.getMethod "Remove" t
      m.Invoke(o, [|item|])
      |> unbox<bool>

    let add t item o =
      let m = Type.getMethod "Add" t
      m.Invoke(o, [|item|])
      |> unbox<unit>

    let contains t item o =
      let m = Type.getMethod "Contains" t
      m.Invoke(o, [|item|])
      |> unbox<bool>

    let isReadOnly t o =
      let p = Type.getProperty "IsReadOnly" t
      p.GetValue(o, [||])
      |> unbox<bool>

    let count t o =
      let p = Type.getProperty "Count" t
      p.GetValue(o, [||])
      |> unbox<int>

    let cast (t: Type) =
      if t = null then None
      elif Type.isGenericType t then
        let ps = Type.getGenericArguments t
        if Array.length ps = 1 then
          let cx = collection.MakeGenericType(ps)
          if Type.isAssignableFrom cx t then Some cx
          else None
        else None
      else None

  [<RequireQualifiedAccess>]
  module IList =

    let private iList = typedefof<IList<_>>

    let private makeIListType (t: Type) =
      iList.MakeGenericType(Type.getGenericArguments t)

    let cast (t: Type) =
      if t = null then None
      elif Type.isGenericType t then
        let ps = Type.getGenericArguments t
        if Array.length ps = 1 then
          let cx = iList.MakeGenericType(ps)
          if Type.isAssignableFrom cx t then Some cx
          else None
        else None
      else None

    let item (t: Type) (index: int) o =
      let p = Type.getProperty "Item" (makeIListType t)
      p.GetValue(o, [|index|])

  [<RequireQualifiedAccess>]
  module FSharpList =

    let private fsharpList = typedefof<_ list>

    let private makeFSharpListType (t: Type) =
      fsharpList.MakeGenericType(Type.getGenericArguments t)

    let cast (t: Type) =
      if t = null then None
      elif Type.isGenericType t then
        let ps = Type.getGenericArguments t
        if Array.length ps = 1 then
          let cx = fsharpList.MakeGenericType(ps)
          if Type.isAssignableFrom cx t then Some cx
          else None
        else None
      else None

    let length t o =
      let p = Type.getProperty "Length" t
      p.GetValue(o, [||])
      |> unbox<int>

    let item (t: Type) (index: int) o =
      let p = Type.getProperty "Item" t
      p.GetValue(o, [|index|])

  let tryFindAllAssignable xs =
    xs
    |> Seq.map ICollection.cast
    |> Seq.reduce (fun acc t ->
      match (acc, t) with
      | Some acc, Some t ->
        if Type.isAssignableFrom acc t then Some acc
        elif Type.isAssignableFrom t acc then Some t
        else None
      | _ -> None)
    |> function
    | Some v -> Some v
    | None ->
      xs
      |> Seq.map FSharpList.cast
      |> Seq.reduce (fun acc t -> if acc = t then acc else None)

[<AutoOpen>]
module CollectionSyntax =

  open Collection

  let (|IsCollection|_|) (o: obj) =
    match o with
    | null -> None
    | :? IList as o -> Some(NonGenericCollection o)
    | _ ->
      let t = o.GetType()
      match ICollection.cast t with
      | Some t ->
        if ICollection.isReadOnly t o then Some(ImmutableGenericCollection(o, t))
        else Some(MutableGenericCollection(o, t))
      | None ->
        match FSharpList.cast t with
        | Some t -> Some(FSharpList(o, t))
        | None -> None
