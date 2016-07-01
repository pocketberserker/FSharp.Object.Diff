namespace FSharp.Object.Diff

open System
open System.Collections.Generic

type internal PropertyId = {
  Type: Type
  Property: string
}

type TypePropertyIdentityStrategyResolver() =

  let strategies = Dictionary<PropertyId, IdentityStrategy>()

  let isQualified (node: DiffNode) =
    if node.IsPropertyAware then
      if node.ParentNode = null || node.ParentNode.Type = null then false
      elif node.PropertyName = null then false
      else true
    else false

  member __.Resolve(node: DiffNode) =
    if isQualified node then
      match strategies.TryGetValue({ Type = node.ParentNode.Type; Property = node.PropertyName }) with
      | true, v -> v
      | false, _ -> null
    else null

  member __.SetStrategy(identityStrategy, typ, [<ParamArray>] properties: string []) =
    for property in properties do
      strategies.Add({ Type = typ; Property = property }, identityStrategy)
