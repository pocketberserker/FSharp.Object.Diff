﻿namespace FSharp.Object.Diff

open System
open System.Collections
open System.Collections.Generic
open Dictionary

type Accessor =
  abstract member ElementSelector: ElementSelector
  abstract member Get: obj -> obj
  abstract member Set: obj * obj -> unit
  abstract member Unset: obj -> unit

type RootAccessor = RootAccessor
with
  override __.ToString() = "root element"
  interface Accessor with
    member __.ElementSelector = RootElementSelector.Instance
    member __.Get(target) = target
    member __.Set(_, _) = raise <| NotSupportedException()
    member __.Unset(_) = raise <| NotSupportedException()

type TypeAwareAccessor =
  inherit Accessor
  abstract member Type: Type

type CategoryAware =
  abstract member CategoriesFromAttribute: Set<string>

type PropertyAwareAccessor =
  inherit TypeAwareAccessor
  inherit CategoryAware
  abstract member PropertyName: string
  abstract member PropertyAttributes: Attribute seq
  abstract member GetPropertyAttribute<'T when 'T :> Attribute and 'T : null> : unit -> 'T

type CollectionItemAccessor(referenceItem: obj, identityStrategy: IdentityStrategy) =

  let objectAsCollection: obj -> Choice<IList option, IEnumerable, ArgumentException> = function
  | null -> Choice1Of3 None
  | :? IList as l -> Choice1Of3(Some l)
  | :? IEnumerable as e -> Choice2Of3 e
  | o -> Choice3Of3(ArgumentException(o.GetType().FullName))

  let remove (xs: IList) =
    let rec inner index =
      if index >= xs.Count then ()
      else
        let x = xs.[index]
        if x <> null && identityStrategy.Equals(x, referenceItem) then
          xs.Remove(x) |> ignore
        else inner (index + 1)
    inner 0

  new(referenceItem) = CollectionItemAccessor(referenceItem, EqualsIdentityStrategy :> IdentityStrategy)

  member __.ElementSelector =
    let selector = CollectionItemElementSelector(referenceItem)
    if identityStrategy = null then selector else selector.WithIdentityStrategy(identityStrategy)
    :> ElementSelector

  member private __.TryGet(target: obj) =

    let rec inner (e: IEnumerator) =
      if e.MoveNext() then
        if e.Current <> null && identityStrategy.Equals(e.Current, referenceItem) then
          Some e.Current
        else inner e
      else None
        
    match objectAsCollection target with
    | Choice1Of3 None -> Choice1Of2 None
    | Choice1Of3(Some cs) -> cs.GetEnumerator() |> inner |> Choice1Of2
    | Choice2Of3 e -> e.GetEnumerator() |> inner |> Choice1Of2
    | Choice3Of3 e -> Choice2Of2 e

  member this.Get(target: obj) =
    match this.TryGet(target) with
    | Choice1Of2 None -> null
    | Choice1Of2(Some v) -> v
    | Choice2Of2 e -> raise e

  member __.Unset(target: obj) =
    match objectAsCollection target with
    | Choice1Of3 None -> ()
    | Choice1Of3(Some targetCollection) ->
      remove targetCollection
    | Choice2Of3 _ ->
      target.GetType().FullName
      |> failwithf "%s can't remove value."
    | Choice3Of3 e -> raise e

  member this.Set(target: obj, value: obj) =
    match objectAsCollection target with
    | Choice1Of3 None -> ()
    | Choice1Of3(Some targetCollection) ->
      let previous = this.Get(target)
      if previous <> null then this.Unset(target)
      targetCollection.Add(value) |> ignore
    | Choice2Of3 _ ->
      target.GetType().FullName
      |> failwithf "%s can't add and remove value."
    | Choice3Of3 e -> raise e

  override this.ToString() =
    "collection item " + this.ElementSelector.ToString()

  member __.Type = if referenceItem <> null then referenceItem.GetType() else null

  interface TypeAwareAccessor with
    member this.Type = this.Type
    member this.ElementSelector = this.ElementSelector
    member this.Get(target) = this.Get(target)
    member this.Set(target, value) = this.Set(target, value)
    member this.Unset(target) = this.Unset(target)

type DictionaryEntryAccessor(referenceKey: obj) =

  let objectToDictionary: obj -> Choice<DictionaryWrapper option, ArgumentException> = function
  | null -> Choice1Of2 None
  | Dictionary d -> Choice1Of2(Some d)
  | o -> Choice2Of2(ArgumentException(o.GetType().FullName))

  member __.ElementSelector = DictionaryKeyElementSelector(referenceKey) :> ElementSelector

  override this.ToString() =
    "map key " + this.ElementSelector.ToString()

  member __.GetKey(target: Dictionary<obj, obj>) =
    if target = null then null
    else
      match target.Keys |> Seq.tryFind ((=) referenceKey) with
      | Some k -> k
      | None -> null

  member __.Get(target: obj) =
    match objectToDictionary target with
    | Choice1Of2(Some(NonGeneric target)) ->
      target.[referenceKey]
    | Choice1Of2(Some(MutableGeneric(target, t) | ImmutableGeneric(target, t))) ->
      Dictionary.Generic.get t referenceKey target
    | Choice1Of2 None -> null
    | Choice2Of2 e -> raise e

  member __.Set(target: obj, value: obj) =
    match objectToDictionary target with
    | Choice1Of2 None -> ()
    | Choice1Of2(Some (NonGeneric target)) ->
      if target.Contains(referenceKey) then target.Remove(referenceKey) |> ignore
      target.Add(referenceKey, value)
    | Choice1Of2(Some (MutableGeneric(target, t))) ->
      if Dictionary.Generic.contains t referenceKey target then
        Dictionary.Generic.remove t referenceKey target |> ignore
      Dictionary.Generic.add t referenceKey value target
    | Choice1Of2(Some (ImmutableGeneric _)) ->
      target.GetType().FullName
      |> failwithf "%s can't add and remove value."
    | Choice2Of2 e -> raise e

  member __.Unset(target: obj) =
    match objectToDictionary target with
    | Choice1Of2 None -> ()
    | Choice1Of2(Some (NonGeneric target)) ->
      target.Remove(referenceKey) |> ignore
    | Choice1Of2(Some (MutableGeneric(target, t))) ->
      ()
    | Choice1Of2(Some (ImmutableGeneric _)) ->
      target.GetType().FullName
      |> failwithf "%s can't remove value."
    | Choice2Of2 e -> raise e

  interface Accessor with
    member this.ElementSelector = this.ElementSelector
    member this.Get(target) = this.Get(target)
    member this.Set(target, value) = this.Set(target, value)
    member this.Unset(target) = this.Unset(target)

type Instances(sourceAccessor: Accessor, working: obj, base_: obj, fresh: obj) =

  static member Of(sourceAccessor: Accessor, working: 'T, base_: 'T, fresh: 'T) =
    Instances(sourceAccessor, working, base_, fresh)

  static member Of(sourceAccessor: Accessor, working: 'T, base_: 'T) =
    let fresh = if box working <> null then Type.FreshInstanceOf<'T>() else null
    Instances(sourceAccessor, working, base_, fresh)

  static member Of(working: 'T, base_: 'T) =
    let fresh =
      match box working with
      | null -> null
      | _ -> Type.FreshInstanceOf<'T>()
    Instances(RootAccessor, working, base_, fresh)

  member __.SourceAccessor = sourceAccessor

  abstract member Access: Accessor -> Instances
  override __.Access(accessor) =
    Instances(accessor, accessor.Get(working), accessor.Get(base_), accessor.Get(fresh))

  member __.Working = working
  member __.TryGetWorking<'T>() = if working <> null && working :? 'T then Some(working :?> 'T) else None
  member __.Base = base_
  member __.TryGetBase<'T>() = if base_ <> null && base_ :? 'T then Some(base_ :?> 'T) else None
  member this.Fresh: obj =
    if fresh = null then
      if Type.isPrimitive this.Type then Activator.CreateInstance(this.Type)
      else fresh
    else
      fresh
  member this.GetFresh(typ: Type) =
    let o = this.Fresh
    if o <> null then Convert.ChangeType(o, typ)
    else null

  member __.TryToGetTypeFromSourceAccessor() =
    match sourceAccessor with
    | :? TypeAwareAccessor as accessor -> accessor.Type
    | _ -> null

  member __.AreEqual = obj.IsEqual(base_, working)
  member __.AreSame = Object.ReferenceEquals(working, base_)
  member __.AreNull = working = null && base_ = null

  abstract member Type: Type
  default this.Type =
    let types = Type.TypesOf(working, base_, fresh) |> Seq.toList
    let sourceAccessorType = this.TryToGetTypeFromSourceAccessor()
    match types with
    | _ when Type.isPrimitive sourceAccessorType -> sourceAccessorType
    | [] -> null
    | [t] -> t
    | _::_ ->
      if typeof<IDictionary>.AllAssignableFrom(types) then
        typeof<IDictionary>
      else
        let ts =
          types
          |> List.map Dictionary.Generic.cast
          |> List.reduce (fun acc t ->
            match (acc, t) with
            | Some acc, Some t ->
              if acc.IsAssignableFrom(t) then Some acc
              elif t.IsAssignableFrom(acc) then Some t
              else None
            | _ -> None)
        match ts with
        | Some t -> t
        | _ when typeof<IEnumerable>.AllAssignableFrom(types) ->
          typeof<IEnumerable>
        | _ ->
          match Type.mostSpecificSharedType types with
          | Some sharedType -> sharedType
          | None when sourceAccessorType <> null -> sourceAccessorType
          | None ->
            types
            |> sprintf "Detected instances of different types %A. Instances must either be null or have the exact same type."
            |> ArgumentException
            |> raise

  member this.IsPrimitiveType = this.Type <> null && this.Type.IsPrimitive

  member this.HasBeenAdded =
    if working <> null && base_ = null then true
    elif this.IsPrimitiveType && Object.IsEqual(this.Fresh, base_) && (not <| Object.IsEqual(base_, working)) then true
    else false

  member this.HasBeenRemoved =
    if base_ <> null && working = null then true
    elif this.IsPrimitiveType && Object.IsEqual(this.Fresh, working) && (not <| Object.IsEqual(base_, working)) then true
    else false
