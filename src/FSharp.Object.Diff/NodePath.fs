namespace FSharp.Object.Diff

open System
open System.Text
open System.Collections.Generic
open Printf

type AppendableBuilder =
  abstract member Element: ElementSelector -> AppendableBuilder
  abstract member PropertyName: string * [<ParamArray>] names: string[] -> AppendableBuilder
  abstract member CollectionItem: 'T -> AppendableBuilder
  abstract member MapKey: 'T -> AppendableBuilder
  abstract member Build: unit -> NodePath

and internal AppendableBuilderImpl(elementSelectors: ResizeArray<ElementSelector>) =
  interface AppendableBuilder with
    member this.Element(elementSelector) =
      elementSelectors.Add(elementSelector)
      this :> AppendableBuilder
    member this.PropertyName(name, [<ParamArray>] names) =
      elementSelectors.Add(BeanPropertyElementSelector(name))
      for name in names do elementSelectors.Add(BeanPropertyElementSelector(name))
      this :> AppendableBuilder
    member this.CollectionItem(item) =
      elementSelectors.Add(CollectionItemElementSelector(item))
      this :> AppendableBuilder
    member this.MapKey(key) =
      Assert.notNull "key" key
      elementSelectors.Add(MapKeyElementSelector(key))
      this :> AppendableBuilder
    member this.Build() =
      match List.ofSeq elementSelectors with
      | [] -> raise <| InvalidOperationException("A property path cannot be empty")
      | x :: _ when not (x :? RootElementSelector) ->
        raise <| InvalidOperationException("A property path must start with a root element")
      | elementSelectors when (elementSelectors |> List.filter (fun x -> x :? RootElementSelector) |> List.length) > 1 ->
        raise <| InvalidOperationException("A property path cannot contain multiple root elements")
      | elementSelectors -> NodePath(elementSelectors)

and [<AllowNullLiteral>] NodePath(elementSelectors: ElementSelector list) =

  member __.ElementSelectors = elementSelectors

  member __.LastElementSelector = Seq.last elementSelectors

  member __.IsParentOf(nodePath: NodePath) =
    let other = nodePath.ElementSelectors
    let l = List.length elementSelectors
    if l < List.length other then
      [ for i in 0 .. l - 1 -> List.nth other i ] = elementSelectors
    else false

  member __.IsChildOf(nodePath: NodePath) =
    let other = nodePath.ElementSelectors
    let l = List.length other
    if List.length elementSelectors > l then
      [ for i in 0 .. l - 1 -> List.nth elementSelectors i ] = other
    else false

  override __.GetHashCode() = hash elementSelectors

  override this.Equals(other) =
    match other with
    | null -> false
    | :? NodePath as other ->
      if Object.ReferenceEquals(this, other) then true
      else elementSelectors = other.ElementSelectors
    | _ -> false

  override __.ToString() =
    let builder = StringBuilder()
    let rec loop (previous: ElementSelector option) (selectors: ElementSelector list) =
      match previous, selectors with
      | _, [] -> ()
      | _, x :: xs when (x :? RootElementSelector) ->
        builder.Append("/") |> ignore
        loop (Some x) xs
      | _, x :: xs when (x :? CollectionItemElementSelector) || (x :? MapKeyElementSelector) ->
        bprintf builder "%O" x
        loop (Some x) xs
      | Some p, x :: xs when (p :? RootElementSelector) ->
        bprintf builder "%O" x
        loop (Some x) xs
      | _, x :: xs ->
        bprintf builder "/%O" x
        loop (Some x) xs
    loop None elementSelectors
    builder.ToString()

  static member StartBuildingFrom(nodePath: NodePath) =
    Assert.notNull "propertyPath" nodePath
    AppendableBuilderImpl(ResizeArray(nodePath.ElementSelectors))
    :> AppendableBuilder

  static member StartBuilding() =
    let elementSelectors = ResizeArray()
    elementSelectors.Add(RootElementSelector.Instance)
    AppendableBuilderImpl(elementSelectors)
    :> AppendableBuilder

  static member With(propertyName: string, [<ParamArray>] additionalPropertyNames) =
    NodePath.StartBuilding()
      .PropertyName(propertyName, additionalPropertyNames)
      .Build()

  static member WithRoot() = NodePath.StartBuilding().Build()

  member this.Matches(nodePath: NodePath) = nodePath.Equals(this)

  member this.CompareTo(other: NodePath) =
    let distance = List.length this.ElementSelectors - List.length other.ElementSelectors
    if distance = 0 then if this.Matches(other) then 0 else 1
    elif distance > 0 then 1
    else -1

  interface IComparable<NodePath> with
    member this.CompareTo(other) = this.CompareTo(other)

  interface IComparable with
    member this.CompareTo(other) =
      match other with
      | :? NodePath as other -> this.CompareTo(other)
      | _ -> invalidArg "other" "cannot compare values of different types"

type NodePathValueHolderCollector<'T> =
  abstract member It: NodePath * 'T -> unit

[<Obsolete("The ConfigNode provides a much more powerful way to store values for NodePaths.")>]
type NodePathValueHolder<'T when 'T : equality>() =

  let mutable value: 'T option = None
  let elementValueHolders = Dictionary<ElementSelector, NodePathValueHolder<'T>>()

  let valueHolderForElementSelector selector =
    match elementValueHolders.TryGetValue(selector) with
    | true, value -> Some value
    | false, _ -> None

  member private __.Put(elementSelectors, value) =
    match elementSelectors with
    | [] -> ()
    | x::xs ->
      let nodePathValueHolder =
        match valueHolderForElementSelector x with
        | None ->
          let nodePathValueHolder = NodePathValueHolder<'T>()
          elementValueHolders.Add(x, nodePathValueHolder)
          nodePathValueHolder
        | Some v -> v
      match xs with
      | [] -> nodePathValueHolder.Value <- value
      | _ -> nodePathValueHolder.Put(xs, value)

  member this.Put(nodePath:NodePath, value) =
    this.Put(nodePath.ElementSelectors, value)
    this

  member internal __.Value with get() = value and set(v) = value <- v

  member private __.Visit(acc: ResizeArray<'T>, selectors) =
    value |> Option.iter acc.Add
    match selectors with
    | x::xs ->
      match valueHolderForElementSelector(x) with
      | Some valueHolder -> valueHolder.Visit(acc, xs)
      | None -> None
    | _ -> value

  member this.ValueForNodePath(nodePath: NodePath) =
    this.Visit(ResizeArray(), nodePath.ElementSelectors)

  member this.AccumulatedValuesForNodePath(nodePath: NodePath) =
    let acc = ResizeArray()
    this.Visit(acc, nodePath.ElementSelectors) |> ignore
    List.ofSeq acc

  member __.ContainsValue(v) =
    match value with
    | Some value when value = v -> true
    | _ ->
      elementValueHolders.Values
      |> Seq.exists (fun e -> e.ContainsValue(v))

  member private __.Collect(nodePath: NodePath option, collector: NodePathValueHolderCollector<_>) =
    match nodePath, value with
    | Some nodePath, Some value -> collector.It(nodePath, value)
    | _ ->
      for KeyValue(elementSelector, valueHolder) in elementValueHolders do
        let childNodePath =
          match elementSelector with
          | :? RootElementSelector -> NodePath.WithRoot()
          | _ -> NodePath.StartBuildingFrom(match nodePath with None -> null | Some n -> n).Element(elementSelector).Build()
        valueHolder.Collect(Some childNodePath, collector)

  member this.Collect(collector) =
    this.Collect(None, collector)

  override this.ToString() =
    let sb = StringBuilder()
    this.Collect({ new NodePathValueHolderCollector<_> with
      member __.It(path, value) =
        bprintf sb "%A => %A" path value
        sb.AppendLine() |> ignore
    })
    sb.ToString()
