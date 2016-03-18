namespace FSharp.Object.Diff

open System

type PrimitiveDefaultValueMode =
  | Assigned
  | UnAssigned

type ComparisonStrategy =
  abstract member Compare: DiffNode * obj * obj -> unit

type ComparisonConfigurerOf =
  abstract member ToUse: ComparisonStrategy -> ComparisonConfigurer
  abstract member ToUseEqualsMethod: unit -> ComparisonConfigurer
  abstract member ToUseEqualsMethodOfValueProvidedByMethod: string -> ComparisonConfigurer
  abstract member ToUseCompareToMethod: unit -> ComparisonConfigurer

and ComparisonConfigurerOfPrimitiveTypes =
  abstract member ToTreatDefaultValuesAs: PrimitiveDefaultValueMode -> ComparisonConfigurer

and ComparisonConfigurer =
  abstract member OfNode: NodePath -> ComparisonConfigurerOf
  abstract member OfTyoe: Type -> ComparisonConfigurer
  abstract member OfPrimitiveTypes: unit -> ComparisonConfigurerOfPrimitiveTypes

type ComparisonStrategyResolver =
  abstract member ResolveComparisonStrategy: DiffNode -> ComparisonStrategy

type PrimitiveDefaultValueModeResolver =
  abstract member ResolvePrimitiveDefaultValueMode: DiffNode -> PrimitiveDefaultValueMode

