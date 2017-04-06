namespace FSharp.Object.Diff

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

  let objectAsCollection: obj -> Choice<CollectionWrapper option, ArgumentException> = function
  | null -> Choice1Of2 None
  | IsCollection c -> Choice1Of2(Some c)
  | o -> Choice2Of2(ArgumentException(o.GetType().FullName))

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
    | Choice1Of2 None -> Choice1Of2 None
    | Choice1Of2(Some(NonGenericCollection cs)) ->
      match index with
      | Some index when index < cs.Count -> Some cs.[index]
      | Some _ -> None
      | _ -> cs.GetEnumerator() |> inner None 0
      |> Choice1Of2
    | Choice1Of2(Some(MutableGenericCollection(cs, t) | ImmutableGenericCollection(cs, t))) ->
      Choice1Of2 <|
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
    | Choice1Of2(Some(FSharpList(cs, t))) ->
      match index with
      | Some index when index < Collection.FSharpList.length t cs -> Collection.FSharpList.item t index cs |> Some
      | Some _ -> None
      | _ -> (cs :?> IEnumerable).GetEnumerator() |> inner None 0
      |> Choice1Of2
    | Choice2Of2 e -> Choice2Of2 e

  member this.Get(target: obj) =
    match this.TryGet(target) with
    | Choice1Of2 None -> null
    | Choice1Of2(Some v) -> v
    | Choice2Of2 e -> raise e

  member __.Unset(target: obj) =
    match objectAsCollection target with
    | Choice1Of2 None -> ()
    | Choice1Of2(Some(NonGenericCollection target)) ->
      remove target
    | Choice1Of2(Some(MutableGenericCollection(target, t))) when Collection.IList.cast (target.GetType()) |> Option.isSome ->
      removeGeneric t target
    | Choice1Of2(Some _) ->
      target.GetType().FullName
      |> failwithf "%s can't remove value."
    | Choice2Of2 e -> raise e

  member this.Set(target: obj, value: obj) =
    match objectAsCollection target with
    | Choice1Of2 None -> ()
    | Choice1Of2(Some(NonGenericCollection targetCollection)) ->
      let previous = this.Get(target)
      if previous <> null then this.Unset(target)
      targetCollection.Add(value) |> ignore
    | Choice1Of2(Some(MutableGenericCollection(targetCollection, t))) ->
      let previous = this.Get(target)
      if previous <> null then this.Unset(target)
      Collection.ICollection.add t value targetCollection
    | Choice1Of2 _ ->
      target.GetType().FullName
      |> failwithf "%s can't add and remove value."
    | Choice2Of2 e -> raise e

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
  | IsDictionary d -> Choice1Of2(Some d)
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
    | Choice1Of2(Some(NonGenericDictionary target)) ->
      target.[referenceKey]
    | Choice1Of2(Some(MutableGenericDictionary(target, t) | ImmutableGenericDictionary(target, t))) ->
      if Dictionary.IDictionary.containsKey t referenceKey target then
        Dictionary.IDictionary.get t referenceKey target
      else null
    | Choice1Of2 None -> null
    | Choice2Of2 e -> raise e

  member __.Set(target: obj, value: obj) =
    match objectToDictionary target with
    | Choice1Of2 None -> ()
    | Choice1Of2(Some (NonGenericDictionary target)) ->
      if target.Contains(referenceKey) then target.Remove(referenceKey) |> ignore
      target.Add(referenceKey, value)
    | Choice1Of2(Some (MutableGenericDictionary(target, t))) ->
      if Dictionary.IDictionary.containsKey t referenceKey target then
        Dictionary.IDictionary.remove t referenceKey target |> ignore
      Dictionary.IDictionary.add t referenceKey value target
    | Choice1Of2(Some (ImmutableGenericDictionary _)) ->
      target.GetType().FullName
      |> failwithf "%s can't add and remove value."
    | Choice2Of2 e -> raise e

  member __.Unset(target: obj) =
    match objectToDictionary target with
    | Choice1Of2 None -> ()
    | Choice1Of2(Some (NonGenericDictionary target)) ->
      target.Remove(referenceKey) |> ignore
    | Choice1Of2(Some (MutableGenericDictionary(target, t))) ->
      Dictionary.IDictionary.remove t referenceKey target |> ignore
    | Choice1Of2(Some (ImmutableGenericDictionary _)) ->
      target.GetType().FullName
      |> failwithf "%s can't remove value."
    | Choice2Of2 e -> raise e

  interface Accessor with
    member this.ElementSelector = this.ElementSelector
    member this.Get(target) = this.Get(target)
    member this.Set(target, value) = this.Set(target, value)
    member this.Unset(target) = this.Unset(target)

type Instances(sourceAccessor: Accessor, working: obj, base_: obj, fresh: obj) =

  let mutable parent: Instances option = None

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
