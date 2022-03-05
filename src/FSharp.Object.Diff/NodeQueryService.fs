namespace FSharp.Object.Diff

type NodeQueryService =
  inherit CategoryResolver
  inherit IsIntrospectableResolver
  inherit IsIgnoredResolver
  inherit IsReturnableResolver
  inherit ComparisonStrategyResolver
  inherit PrimitiveDefaultValueModeResolver

type DefaultNodeQueryService(
                             categoryResolver: CategoryResolver,
                             introspectableResolver: IsIntrospectableResolver,
                             ignoredResolver: IsIgnoredResolver,
                             returnableResolver: IsReturnableResolver,
                             comparisonStrategyResolver: ComparisonStrategyResolver,
                             primitiveDefaultValueModeResolver: PrimitiveDefaultValueModeResolver
  ) =

  interface NodeQueryService with
    member __.ResolveCategories(node) = categoryResolver.ResolveCategories(node)
    member __.IsIntrospectable(node) = introspectableResolver.IsIntrospectable(node)
    member __.IsIgnored(node) = ignoredResolver.IsIgnored(node)
    member __.IsReturnable(node) = returnableResolver.IsReturnable(node)
    member __.ResolveComparisonStrategy(node) = comparisonStrategyResolver.ResolveComparisonStrategy(node)
    member __.ResolvePrimitiveDefaultValueMode(node) = primitiveDefaultValueModeResolver.ResolvePrimitiveDefaultValueMode(node)

type DifferFactory =
  abstract member CreateDiffer: DifferDispatcher * NodeQueryService -> Differ
