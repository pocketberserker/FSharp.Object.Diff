namespace FSharp.Object.Diff

open System

type PrimitiveDefaultValueMode =
  | Assigned
  | UnAssigned

[<AllowNullLiteral>]
type ComparisonStrategy =
  abstract member Compare: DiffNode * obj * obj -> unit

type ComparisonStrategyResolver =
  abstract member ResolveComparisonStrategy: DiffNode -> ComparisonStrategy

type PrimitiveDefaultValueModeResolver =
  abstract member ResolvePrimitiveDefaultValueMode: DiffNode -> PrimitiveDefaultValueMode

type EqualsOnlyComparisonStrategy(equalsValueProviderMethod: string) =

  static let access (target: obj) methodName =
    if target = null then null
    else
      let m = target.GetType().GetMethod(methodName)
      m.Invoke(target, [||])

  new() = EqualsOnlyComparisonStrategy(null)

  interface ComparisonStrategy with
    member __.Compare(node, working, base_) =
      let result =
        if equalsValueProviderMethod <> null then
          let workingValue = access working equalsValueProviderMethod
          let baseValue = access base_ equalsValueProviderMethod
          Object.IsEqual(workingValue, baseValue)
        else Object.IsEqual(working, base_)
      if result then node.State <- Untouched
      else node.State <- Changed
