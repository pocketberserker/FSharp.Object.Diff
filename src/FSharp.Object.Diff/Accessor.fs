﻿namespace FSharp.Object.Diff

open System
open System.Collections
open System.Collections.Generic

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

type CollectionItemAccessor(referenceItem: obj, index: int option, identityStrategy: IdentityStrategy) =

  let objectAsCollection: obj -> Result<CollectionWrapper option, ArgumentException> = function
  | null -> Ok None
  | IsCollection c -> Ok(Some c)
  | o -> Error(ArgumentException(o.GetType().FullName))

  let remove (xs: IList) =
    let rec inner index =
      if index >= xs.Count then ()
      else
        let x = xs.[index]
        if x <> null && identityStrategy.Equals(x, referenceItem) then
          xs.Remove(x) |> ignore
        else inner (index + 1)
    inner 0

  let removeGeneric (t: Type) (xs: obj) =
    let rec inner index =
      if index >= Collection.ICollection.count t xs then ()
      else
        let x = Collection.IList.item t index xs
        if x <> null && identityStrategy.Equals(x, referenceItem) then
          Collection.ICollection.remove t x xs |> ignore
        else inner (index + 1)
    inner 0

  new(referenceItem) = CollectionItemAccessor(referenceItem, None, EqualsIdentityStrategy :> IdentityStrategy)

  member __.Index = index

  member __.ElementSelector =
    let selector = CollectionItemElementSelector(referenceItem, index)
    if identityStrategy = null then selector else selector.WithIdentityStrategy(identityStrategy)
    :> ElementSelector

  member __.TryGet(target: obj) =

    let rec inner (index: int option) count (e: IEnumerator) =
      if e.MoveNext() then
        if e.Current <> null && identityStrategy.Equals(e.Current, referenceItem) then
          match index with
          | Some index when count = index -> Some e.Current
          | Some _ -> inner index (count + 1) e
          | None -> Some e.Current
        else inner index (count + 1) e
      else None

    match objectAsCollection target with
    | Ok None -> Ok None
    | Ok(Some(NonGenericCollection cs)) ->
      match index with
      | Some index when index < cs.Count -> Some cs.[index]
      | Some _ -> None
      | _ -> cs.GetEnumerator() |> inner None 0
      |> Ok
    | Ok(Some(MutableGenericCollection(cs, t) | ImmutableGenericCollection(cs, t))) ->
      Ok <|
      match Collection.IList.cast t with
      | Some _ ->
        match index with
        | Some index when index < Collection.ICollection.count t cs -> Collection.IList.item t index cs |> Some
        | Some _ -> None
        | _ -> (cs :?> IEnumerable).GetEnumerator() |> inner None 0
      | None ->
        match index with
        | Some index when index < Collection.ICollection.count t cs -> (cs :?> IEnumerable).GetEnumerator() |> inner (Some index) 0
        | Some _ -> None
        | _ -> (cs :?> IEnumerable).GetEnumerator() |> inner None 0
    | Ok(Some(FSharpList(cs, t))) ->
      match index with
      | Some index when index < Collection.FSharpList.length t cs -> Collection.FSharpList.item t index cs |> Some
      | Some _ -> None
      | _ -> (cs :?> IEnumerable).GetEnumerator() |> inner None 0
      |> Ok
    | Error e -> Error e

  member this.Get(target: obj) =
    match this.TryGet(target) with
    | Ok None -> null
    | Ok(Some v) -> v
    | Error e -> raise e

  member __.Unset(target: obj) =
    match objectAsCollection target with
    | Ok None -> ()
    | Ok(Some(NonGenericCollection target)) ->
      remove target
    | Ok(Some(MutableGenericCollection(target, t))) when Collection.IList.cast (target.GetType()) |> Option.isSome ->
      removeGeneric t target
    | Ok(Some _) ->
      target.GetType().FullName
      |> failwithf "%s can't remove value."
    | Error e -> raise e

  member this.Set(target: obj, value: obj) =
    match objectAsCollection target with
    | Ok None -> ()
    | Ok(Some(NonGenericCollection targetCollection)) ->
      let previous = this.Get(target)
      if previous <> null then this.Unset(target)
      targetCollection.Add(value) |> ignore
    | Ok(Some(MutableGenericCollection(targetCollection, t))) ->
      let previous = this.Get(target)
      if previous <> null then this.Unset(target)
      Collection.ICollection.add t value targetCollection
    | Ok _ ->
      target.GetType().FullName
      |> failwithf "%s can't add and remove value."
    | Error e -> raise e

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

  let objectToDictionary: obj -> Result<DictionaryWrapper option, ArgumentException> = function
  | null -> Ok None
  | IsDictionary d -> Ok(Some d)
  | o -> Error(ArgumentException(o.GetType().FullName))

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
    | Ok(Some(NonGenericDictionary target)) ->
      target.[referenceKey]
    | Ok(Some(MutableGenericDictionary(target, t) | ImmutableGenericDictionary(target, t))) ->
      if Dictionary.IDictionary.containsKey t referenceKey target then
        Dictionary.IDictionary.get t referenceKey target
      else null
    | Ok None -> null
    | Error e -> raise e

  member __.Set(target: obj, value: obj) =
    match objectToDictionary target with
    | Ok None -> ()
    | Ok(Some (NonGenericDictionary target)) ->
      if target.Contains(referenceKey) then target.Remove(referenceKey) |> ignore
      target.Add(referenceKey, value)
    | Ok(Some (MutableGenericDictionary(target, t))) ->
      if Dictionary.IDictionary.containsKey t referenceKey target then
        Dictionary.IDictionary.remove t referenceKey target |> ignore
      Dictionary.IDictionary.add t referenceKey value target
    | Ok(Some (ImmutableGenericDictionary _)) ->
      target.GetType().FullName
      |> failwithf "%s can't add and remove value."
    | Error e -> raise e

  member __.Unset(target: obj) =
    match objectToDictionary target with
    | Ok None -> ()
    | Ok(Some (NonGenericDictionary target)) ->
      target.Remove(referenceKey) |> ignore
    | Ok(Some (MutableGenericDictionary(target, t))) ->
      Dictionary.IDictionary.remove t referenceKey target |> ignore
    | Ok(Some (ImmutableGenericDictionary _)) ->
      target.GetType().FullName
      |> failwithf "%s can't remove value."
    | Error e -> raise e

  interface Accessor with
    member this.ElementSelector = this.ElementSelector
    member this.Get(target) = this.Get(target)
    member this.Set(target, value) = this.Set(target, value)
    member this.Unset(target) = this.Unset(target)

type TupleItemAccessor = {
  Accessor: PropertyAwareAccessor
  N: int
}
with
  member this.ElementSelector = TupleItemElementSelector(this.Accessor.PropertyName, this.N) :> ElementSelector
  member this.Get(target) = this.Accessor.Get(target)
  interface PropertyAwareAccessor with
    member this.Type = this.Accessor.Type
    member this.GetPropertyAttribute<'T when 'T :> Attribute and 'T : null>() = this.Accessor.GetPropertyAttribute<'T>()
    member this.PropertyAttributes = this.Accessor.PropertyAttributes
    member this.PropertyName = this.Accessor.PropertyName
    member this.Set(target, value) = this.Accessor.Set(target, value)
    member this.Unset(target) = this.Accessor.Unset(target)
    member this.CategoriesFromAttribute = this.Accessor.CategoriesFromAttribute
    member this.Get(target) = this.Get(target)
    member this.ElementSelector = this.ElementSelector

type Instances(sourceAccessor: Accessor, working: obj, base_: obj, fresh: obj) =

  let mutable parent: Instances option = None

  static member Of(sourceAccessor: Accessor, working: 'T, base_: 'T, fresh: 'T) =
    Instances(sourceAccessor, working, base_, fresh)

  static member Of(sourceAccessor: Accessor, working: 'T, base_: 'T) =
    let fresh = if box working <> null then working.GetType().FreshInstanceOf() else null
    Instances(sourceAccessor, working, base_, fresh)

  static member Of(working: 'T, base_: 'T) =
    let fresh =
      match box working with
      | null -> null
      | _ -> working.GetType().FreshInstanceOf()
    Instances(RootAccessor, working, base_, fresh)

  member __.SourceAccessor = sourceAccessor

  member __.Parent with get() = parent and private set(v) = parent <- v

  abstract member Access: Accessor -> Instances
  override this.Access(accessor) =
    let child = Instances(accessor, accessor.Get(working), accessor.Get(base_), accessor.Get(fresh))
    child.Parent <- Some this
    child

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
    if o <> null then Convert.ChangeType(o, typ, null)
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
        match Dictionary.tryFindAllAssignable types, Collection.tryFindAllAssignable types with
        | Some t, _ -> t
        | _, Some t -> t
        | _ when typeof<IEnumerable>.AllAssignableFrom(types) ->
          typeof<IEnumerable>
        | _ ->
          match Type.mostSpecificSharedType types with
          | Some sharedType -> sharedType
          | None when sourceAccessorType <> null -> sourceAccessorType
          | None -> typeof<obj>

  member this.IsPrimitiveType = Type.isPrimitive this.Type

  member this.HasBeenAdded =
    if working <> null && base_ = null then true
    elif this.IsPrimitiveType && Object.IsEqual(this.Fresh, base_) && (not <| Object.IsEqual(base_, working)) then true
    else false

  member this.HasBeenRemoved =
    if base_ <> null && working = null then true
    elif this.IsPrimitiveType && Object.IsEqual(this.Fresh, working) && (not <| Object.IsEqual(base_, working)) then true
    else false
