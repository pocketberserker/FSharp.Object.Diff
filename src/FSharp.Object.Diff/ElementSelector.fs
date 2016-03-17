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
    | :? RootElementSelector as other when this = other -> true
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
      if this = other then true
      else this.PropertyName = other.PropertyName
    | _ -> false
  override __.GetHashCode() = hash propertyName

type CollectionItemElementSelector(item: obj, identityStrategy: IdentityStrategy) =
  inherit ElementSelector()

  new (item) = CollectionItemElementSelector(item, EqualsIdentityStrategy)

  member internal __.Item = item

  member __.WithIdentityStrategy(identityStrategy) = CollectionItemElementSelector(item, identityStrategy)

  override __.HumanReadableString =
    "[" + String.toSingleLineString item + "]"

  override this.Equals(other) =
    match other with
    | null -> false
    | :? CollectionItemElementSelector as other ->
      if this = other then true
      else
        if item <> null then identityStrategy.Equals(item, other.Item)
        else other.Item <> null
    | _ -> false

  override __.GetHashCode() = 31

[<Sealed>]
type MapKeyElementSelector(key: obj) =
  inherit ElementSelector()

  member internal __.Key = key

  override __.HumanReadableString =
    "{" + String.toSingleLineString key + "}"

  override this.Equals(other) =
    match other with
    | null -> false
    | :? MapKeyElementSelector as other ->
      if this = other then true
      else
        if key <> null then key = other.Key
        else other.Key = null
    | _ -> false

  override __.GetHashCode() = if key <> null then key.GetHashCode() else 0
