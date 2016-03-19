namespace FSharp.Object.Diff

open System
open System.Collections.Generic

type Inclusion =
  | Default
  | Included
  | Excluded

type IsIgnoredResolver =
  abstract member IsIgnored: DiffNode -> bool

type InclusionResolver =
  abstract member GetInclusion: DiffNode -> Inclusion
  abstract member EnablesStrictIncludeMode: unit -> bool

type CategoryInclusionResolver(categoryResolver: CategoryResolver) =

  let categoryInclusions = Dictionary<string, Inclusion>()
  let mutable containsIncluded = false
  let mutable containsExcluded = false

  let isInactive () = not containsIncluded && not containsExcluded

  member __.SetInclusion(category, inclusion) =
    categoryInclusions.Add(category, inclusion)
    containsIncluded <- categoryInclusions.ContainsValue(Included)
    containsExcluded <- categoryInclusions.ContainsValue(Excluded)

  interface InclusionResolver with

    member __.GetInclusion(node) =
      if isInactive () then Default
      else
        categoryResolver.ResolveCategories(node)
        |> Seq.fold (fun resolvedInclusion category ->
          match categoryInclusions.TryGetValue(category) with
          | true, Excluded -> Excluded
          | true, Included -> Included
          | _ -> resolvedInclusion
        ) Default

    member __.EnablesStrictIncludeMode() = containsIncluded

type ValueNode<'T when 'T : equality>(elementSelector: ElementSelector, parent: ValueNode<'T> option) =
  
  let children = Dictionary<ElementSelector, ValueNode<'T>>()

  new() = ValueNode(RootElementSelector.Instance, None)

  member val Value: 'T option = None with get, set

  member __.ElementSelector = elementSelector
  member __.Parent = parent

  member this.GetChild(childSelector: ElementSelector) =
    match childSelector with
    | :? RootElementSelector -> raise <| ArgumentException("A child node can never be the root")
    | _ ->
      match children.TryGetValue(childSelector) with
      | true, value -> value
      | false, _ ->
        let childNode = ValueNode(childSelector, Some this)
        children.Add(childSelector, childNode)
        childNode

  member private this.GetChild(childSelectors: ElementSelector list) =
    match childSelectors with
    | [] -> failwith "oops!"
    | [x] -> this.GetChild(x)
    | x::xs ->
      let child = this.GetChild(x)
      child.GetChild(xs)

  member this.GetNodeForPath(nodePath: NodePath) =
    match parent with
    | None ->
      match nodePath.ElementSelectors with
      | [] | [_] -> this
      | _::xs -> this.GetChild(xs)
    | Some parent -> parent.GetNodeForPath(nodePath)

  member __.HasChild(childSelector) = children.ContainsKey(childSelector)

  member this.HasValue = Option.isSome this.Value
  
  member this.ContainsValue(value) =
    if this.Value |> Option.forall ((=) value) then true
    else children.Values |> Seq.exists (fun child -> child.ContainsValue(value))

  member __.GetClosestParentWithValue() =
    parent
    |> Option.bind (fun parent ->
      if parent.HasValue then Some parent
      else parent.GetClosestParentWithValue()
    )

type NodePathInclusionResolver() =

  let inclusions = ValueNode<Inclusion>()
  let mutable containsIncluded = false
  let mutable containsExcluded = false

  let isInactive () = not containsIncluded && not containsExcluded

  let rec resolveParentInclusion (inclusionNode: ValueNode<Inclusion>) =
    match inclusionNode.GetClosestParentWithValue() with
    | Some parentWithInclusion -> resolveInclusion parentWithInclusion
    | None -> Default

  and resolveInclusion (inclusionNode: ValueNode<Inclusion>) =
    if inclusionNode.Value = Some Excluded then Excluded
    else
      match resolveParentInclusion inclusionNode with
      | (Included | Excluded) as parentInclusion -> parentInclusion
      | Default ->
        if inclusionNode.ContainsValue(Included) then Included
        else Default

  member __.SetInclusion(nodePath, inclusion) =
    inclusions.GetNodeForPath(nodePath).Value <- Some inclusion
    containsIncluded <- inclusions.ContainsValue(Included)
    containsExcluded <- inclusions.ContainsValue(Excluded)

  interface InclusionResolver with

    member __.GetInclusion(node) =
      if isInactive () then Default
      else resolveInclusion (inclusions.GetNodeForPath(node.Path))

    member __.EnablesStrictIncludeMode() = containsIncluded

type PropertyNameInclusionResolver() =

  let propertyNameInclusions = Dictionary<string, Inclusion>()
  let mutable containsIncluded = false
  let mutable containsExcluded = false

  let isInactive () = not containsIncluded && not containsExcluded

  member __.SetInclusion(propertyName, inclusion) =
    propertyNameInclusions.Add(propertyName, inclusion)
    containsIncluded <- propertyNameInclusions.ContainsValue(Included)
    containsExcluded <- propertyNameInclusions.ContainsValue(Excluded)

  member this.GetInclusion(node: DiffNode) =
    if node <> null && not<| isInactive () then
      match propertyNameInclusions.TryGetValue(node.PropertyName) with
      | true, ((Included | Excluded) as inclusion) -> inclusion
      | _ ->
        match this.GetInclusion(node.ParentNode) with
        | Included -> Included
        | _ -> Default
    else Default

  interface InclusionResolver with
    member this.GetInclusion(node) = this.GetInclusion(node)
    member __.EnablesStrictIncludeMode() = containsIncluded

type TypeInclusionResolver() =

  let typeInclusions = Dictionary<Type, Inclusion>()
  let mutable containsIncluded = false
  let mutable containsExcluded = false

  let isInactive () = not (containsIncluded || containsExcluded)

  member __.SetInclusion(typ, inclusion) =
    typeInclusions.Add(typ, inclusion)
    containsIncluded <- typeInclusions.ContainsValue(Included)
    containsExcluded <- typeInclusions.ContainsValue(Excluded)

  interface InclusionResolver with

    member this.GetInclusion(node) =
      if isInactive () then Default
      else
        match node.Type with
        | null -> Default
        | typ ->
          match typeInclusions.TryGetValue(typ) with
          | true, ((Included | Excluded) as inclusion) -> inclusion
          | _ -> Default

    member __.EnablesStrictIncludeMode() = containsIncluded

type TypePropertyConfigInclusionResolver() =

  let inclusions = Dictionary<PropertyId, Inclusion>()

  let isQualified (node: DiffNode) =
    if node.IsProperyAware then
      if node.ParentNode <> null && node.ParentNode.Type = null then false
      elif node.PropertyName = null then false
      else true
    else false

  let hasIncludedSiblings (node: DiffNode) =
    inclusions
    |> Seq.fold (fun result (KeyValue(k, v)) ->
      if result then result
      else k.Type = node.ParentNode.Type && v = Included
    ) false

  member __.SetInclusion(typ, property, inclusion) =
    inclusions.Add({ Type = typ; Property = property }, inclusion)

  interface InclusionResolver with

    member __.GetInclusion(node) =
      if isQualified node then
        let propertyKey = { Type = node.ParentNode.Type; Property = node.PropertyName }
        match inclusions.TryGetValue(propertyKey) with
        | true, Default when hasIncludedSiblings node -> Excluded
        | true, inclusion -> inclusion
        | _ -> Default
      else Default

    member __.EnablesStrictIncludeMode() = false
