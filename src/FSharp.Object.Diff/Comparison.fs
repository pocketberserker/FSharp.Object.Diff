namespace FSharp.Object.Diff

open System

type PrimitiveDefaultValueMode =
  | Assigned
  | UnAssigned

[<AllowNullLiteral>]
type ComparisonStrategy =
  abstract member Compare: DiffNode * Type * obj * obj -> unit

type ComparisonStrategyResolver =
  abstract member ResolveComparisonStrategy: DiffNode -> ComparisonStrategy

type PrimitiveDefaultValueModeResolver =
  abstract member ResolvePrimitiveDefaultValueMode: DiffNode -> PrimitiveDefaultValueMode

type EqualsOnlyComparisonStrategy(propertyName: string) =

  static let access (target: obj) methodName =
    if target = null then null
    else
      let m = target.GetType().GetProperty(methodName)
      m.GetValue(target, [||])

  new() = EqualsOnlyComparisonStrategy(null)

  interface ComparisonStrategy with
    member __.Compare(node, _, working, base_) =
      let result =
        if propertyName <> null then
          let workingValue = access working propertyName
          let baseValue = access base_ propertyName
          Object.IsEqual(workingValue, baseValue)
        else Object.IsEqual(working, base_)
      if result then node.State <- Untouched
      else node.State <- Changed

type ComparableComparisonStrategy = ComparableComparisonStrategy
with
  interface ComparisonStrategy with
    member __.Compare(node, typ, working, base_) =
      if typeof<IComparable>.IsAssignableFrom(typ) then
        if isEqualByComparison (working :?> IComparable) (base_ :?> IComparable) then
          node.State <- Untouched
        else node.State <- Changed

type ObjectDiffPropertyComparisonStrategyResolver =  ObjectDiffPropertyComparisonStrategyResolver
with
  member __.ComparisonStrategyForAttribute(attr: ObjectDiffPropertyAttribute) =
    if attr = null || not attr.EqualsOnly then None
    elif not <| String.IsNullOrEmpty(attr.EqualsOnlyValueProviderProperty) then
      Some(EqualsOnlyComparisonStrategy(attr.EqualsOnlyValueProviderProperty))
    else Some(EqualsOnlyComparisonStrategy())
  member __.ComparisonStrategyForAttribute(attr: ObjectDiffEqualsOnlyAttribute) =
    if attr = null then None
    elif not <| String.IsNullOrEmpty(attr.ValueProviderProperty) then
      Some(EqualsOnlyComparisonStrategy(attr.ValueProviderProperty))
    else Some(EqualsOnlyComparisonStrategy())
