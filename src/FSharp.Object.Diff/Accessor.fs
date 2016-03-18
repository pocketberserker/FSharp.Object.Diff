namespace FSharp.Object.Diff

open System
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
  abstract member GetCategoriesFromAttribute: unit -> Set<string>

type PropertyAwareAccessor =
  inherit TypeAwareAccessor
  inherit CategoryAware
  abstract member PropertyName: string
  //abstract member GetFieldAttributes: unit -> Set<Attribute>
  abstract member GetFieldAttribute: Type -> Attribute
  //abstract member GetReadMethodAttributes: unit -> Set<Attribute>
  abstract member GetReadMethodAttribute: Type -> Attribute

type CollectionItemAccessor(referenceItem: obj, identityStrategy: IdentityStrategy) =

  let objectAsCollection: obj -> ICollection<obj> = function
  | null -> null
  | :? ICollection<obj> as c -> c
  | o -> raise <| ArgumentException(o.GetType().FullName)

  let remove (xs: ICollection<_>) =
    let rec inner index =
      if index >= xs.Count then ()
      else
        let x = Seq.nth index xs
        if x <> null && identityStrategy.Equals(x, referenceItem) then
          xs.Remove(x) |> ignore
        else inner (index + 1)
    inner 0

  new(referenceItem) = CollectionItemAccessor(referenceItem, EqualsIdentityStrategy :> IdentityStrategy)

  member __.ElementSelector =
    let selector = CollectionItemElementSelector(referenceItem)
    if identityStrategy = null then selector else selector.WithIdentityStrategy(identityStrategy)
    :> ElementSelector

  member internal __.TryGet(target) =
    objectAsCollection target
    |> Seq.tryFind (fun item -> item <> null && identityStrategy.Equals(item, referenceItem))

  member this.Get(target) =
    match this.TryGet(target) with
    | Some o -> o
    | None -> null

  member __.Unset(target) =
    let targetCollection = objectAsCollection target
    if targetCollection <> null then
      remove targetCollection

  override this.ToString() =
    "collection item " + this.ElementSelector.ToString()

  interface TypeAwareAccessor with
    member __.Type = if referenceItem <> null then referenceItem.GetType() else null
    member this.ElementSelector = this.ElementSelector
    member this.Get(target) = this.Get(target)
    member this.Set(target, value) =
      let targetCollection = objectAsCollection target
      if targetCollection <> null then
        let previous = this.Get(target)
        if previous <> null then this.Unset(target)
        targetCollection.Add(value)
    member this.Unset(target) = this.Unset(target)

type MapEntryAccessor(referenceKey: obj) =

  let objectToDictionary: obj -> Dictionary<obj, obj> = function
  | null -> null
  | :? Dictionary<obj, obj> as d -> d
  | o -> raise <| ArgumentException(o.GetType().FullName)

  member __.ElementSelector = MapKeyElementSelector(referenceKey) :> ElementSelector

  override this.ToString() =
    "map key " + this.ElementSelector.ToString()

  interface Accessor with
    member this.ElementSelector = this.ElementSelector
    member __.Get(target) =
      let target = objectToDictionary target
      if target <> null then
        target.[referenceKey]
      else null
    member __.Set(target, value) =
      let target = objectToDictionary target
      if target <> null then
        target.Add(referenceKey, value)
    member __.Unset(target) =
      let target = objectToDictionary target
      if target <> null then
        target.Remove(referenceKey) |> ignore

type Instances(sourceAccessor: Accessor, working: obj, base_: obj, fresh: obj) =

  static member Of(sourceAccessor: Accessor, working: 'T, base_: 'T, fresh: 'T) =
    Instances(sourceAccessor, working, base_, fresh)

  member __.SourceAccessor = sourceAccessor

  member __.Access(accessor: Accessor) =
    Instances(accessor, accessor.Get(working), accessor.Get(base_), accessor.Get(fresh))

  member __.Working = working
  member __.GetWorking(typ: Type) = if working <> null then Convert.ChangeType(working, typ) else null
  member __.Base = base_
  member __.GetBase(typ: Type) = if base_ <> null then Convert.ChangeType(base_, typ) else null
  member __.Fresh: obj =
//    if fresh = null then
//      if isPrimitiveNumericType () then 0
//      elif isPrimitiveBooleanType () then false
//      else fresh
//    else
      fresh
  member this.GetFresh(typ: Type) =
    let o = this.Fresh
    if o <> null then Convert.ChangeType(o, typ)
    else null

  member __.TryToGetTypeFromSourceAccessor() =
    match sourceAccessor with
    | :? TypeAwareAccessor as accessor -> accessor.Type
    | _ -> null

  member __.AreEqual() = obj.IsEqual(base_, working)
  member __.AreSame() = Object.ReferenceEquals(working, base_)
  member __.AreNull() = working = null && base_ = null

  member this.Type =
    let types = Type.TypesOf(working, base_, fresh)
    let raise () =
      types
      |> sprintf "Detected instances of different types %A. Instances must either be null or have the exact same type."
      |> ArgumentException
      |> raise
    let sourceAccessorType = this.TryToGetTypeFromSourceAccessor()
    if sourceAccessorType.IsPrimitive then sourceAccessorType
    elif Seq.isEmpty types then null
    elif Seq.length types = 1 then
      types |> Seq.head
    elif Seq.length types > 1 then
      // TODO: implement
      raise ()
    else raise()

  member this.IsPrimitiveType = this.Type <> null && this.Type.IsPrimitive

  member this.HasBeenAdded() =
    if working <> null && base_ = null then true
    elif this.IsPrimitiveType && Object.IsEqual(this.Fresh, working) && (not <| Object.IsEqual(base_, working)) then true
    else false

