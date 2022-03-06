module FSharp.Object.Diff.Tests.TupleDifferTest

open System
open Persimmon
open UseTestNameByReflection
open Foq
open FSharp.Object.Diff

type MockTarget = {
  IntrospectableResolver: Mock<IsIntrospectableResolver>
  TypeInfoResolver: Mock<TypeInfoResolver>
  ReturnableResolver: Mock<IsReturnableResolver>
  ComparisonStrategy: ComparisonStrategy
  ComparisonStrategyResolver: Mock<ComparisonStrategyResolver>
}

type Target = {
  Instances: Instances
  Differ: TupleDiffer
  ComparisonStrategy: ComparisonStrategy
  TypeInfoResolver: TypeInfoResolver
}

let setup (f: MockTarget -> MockTarget) =
  let detector = CircularReferenceDetector(DetectorReferenceMatchingMode.EqualityOperator)
  let comparisonStrategy = Mock<ComparisonStrategy>().Create()
  let target =
    {
      IntrospectableResolver = Mock<IsIntrospectableResolver>()
      TypeInfoResolver = Mock<TypeInfoResolver>()
      ReturnableResolver = Mock<IsReturnableResolver>()
      ComparisonStrategy = comparisonStrategy
      ComparisonStrategyResolver = Mock<ComparisonStrategyResolver>()
    }
    |> f
  let returnableResolver = target.ReturnableResolver.Create()
  let dispatcher =
    DifferDispatcher(
      DifferProvider(),
      Mock<CircularReferenceDetectorFactory>()
        .SetupMethod(fun x -> <@ x.CreateCircularReferenceDetector @>).Returns(detector)
        .Create(),
      Mock<CircularReferenceExceptionHandler>().Create(),
      Mock<IsIgnoredResolver>().Create(),
      returnableResolver,
      Mock<PropertyAccessExceptionHandlerResolver>().Create(),
      Mock<CategoryResolver>()
        .SetupMethod(fun x -> <@ x.ResolveCategories @>).Returns(Set.empty)
        .Create()
    )
  let comparisonStrategyResolver = target.ComparisonStrategyResolver.Create()
  let typeInfoResolver = target.TypeInfoResolver.Create()
  {
    Instances = Instances(RootAccessor, obj(), obj(), obj())
    Differ =
      TupleDiffer(
        dispatcher,
        target.IntrospectableResolver.Create(),
        returnableResolver,
        comparisonStrategyResolver,
        typeInfoResolver
      )
    ComparisonStrategy = comparisonStrategy
    TypeInfoResolver = typeInfoResolver
  }

let ``accepts all object types`` = parameterize {
  source [
    typeof<(int * int)>
    typeof<(int * int * int)>
    typeof<struct (int * int)>
    typeof<struct (int * int * int)>
  ]
  run (fun t -> test {
    let target = setup id
    do! assertPred (target.Differ.Accepts(t))
  })
}

let ``returns added node if working is not null and base is`` = test {
  let target = setup id
  let instances = Instances(RootAccessor, (1, 2), null, null)
  do! assertEquals Added (target.Differ.Compare(DiffNode.Root, instances).State)
}

let ``returns removed node if working is null and base is not`` = test {
  let target = setup id
  let instances = Instances(RootAccessor, null, (1, 2), null)
  do! assertEquals Removed (target.Differ.Compare(DiffNode.Root, instances).State)
}

let ``returns untouched node if working and base are the same instance`` = test {
  let target = setup id
  let o = (1, 2)
  let instances = Instances(RootAccessor, o, o, null)
  do! assertEquals Untouched (target.Differ.Compare(DiffNode.Root, instances).State)
}

