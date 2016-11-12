namespace FSharp.Object.Diff

[<AbstractClass>]
[<StructuredFormatDisplay("{HumanReadableString}")>]
type ElementSelector() =
  abstract member HumanReadableString: string
  override this.ToString() = this.HumanReadableString

[<Sealed>]
type RootElementSelector private () =
  inherit ElementSelector()
  static let instance = RootElementSelector()
  static member Instance = instance :> ElementSelector
  override __.HumanReadableString = ""
  override this.Equals(other) =
    match other with
    | :? RootElementSelector as other when obj.ReferenceEquals(this, other) -> true
    | _ ->
      if other <> null && this.GetType() = other.GetType() then true
      else false
  override __.GetHashCode() = 0

[<Sealed>]
type BeanPropertyElementSelector(propertyName: string) =
  inherit ElementSelector()
  override __.HumanReadableString = propertyName
  member __.PropertyName = propertyName
  override this.Equals(other) =
    match other with
    | null -> false
    | :? BeanPropertyElementSelector as other ->
      if obj.ReferenceEquals(this, other) then true
      else this.PropertyName = other.PropertyName
    | _ -> false
  override __.GetHashCode() = hash propertyName

type CollectionItemElementSelector(item: obj, index: int option, identityStrategy: IdentityStrategy) =
  inherit ElementSelector()

  new (item) = CollectionItemElementSelector(item, None, EqualsIdentityStrategy)
  new (item, index) = CollectionItemElementSelector(item, index, EqualsIdentityStrategy)

  member internal __.Item = item
  member internal __.Index = index

  member __.WithIdentityStrategy(identityStrategy) = CollectionItemElementSelector(item, index, identityStrategy)

  override __.HumanReadableString =
    match index with
    | Some index -> "[" + string index + "]"
    | None -> "[" + String.toSingleLineString item + "(not index)]"

  override this.Equals(other) =
    match other with
    | null -> false
    | :? CollectionItemElementSelector as other ->
      if obj.ReferenceEquals(this, other) then true
      else
        match item, index with
        | null, None -> other.Item = null && other.Index = None
        | null, Some _ -> other.Item = null && other.Index = index
        | _, None -> identityStrategy.Equals(item, other.Item) && other.Index = None
        | _, Some _ -> identityStrategy.Equals(item, other.Item) && other.Index = index
    | _ -> false

  override __.GetHashCode() = 31

[<Sealed>]
type DictionaryKeyElementSelector(key: obj) =
  inherit ElementSelector()

  member __.Key = key

  override __.HumanReadableString =
    "{" + String.toSingleLineString key + "}"

  override this.Equals(other) =
    match other with
    | null -> false
    | :? DictionaryKeyElementSelector as other ->
      if obj.ReferenceEquals(this, other) then true
      else
        if key <> null then key = other.Key
        else other.Key = null
    | _ -> false

  override __.GetHashCode() = if key <> null then key.GetHashCode() else 0
