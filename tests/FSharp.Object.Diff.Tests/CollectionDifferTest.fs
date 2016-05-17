module FSharp.Object.Diff.Tests.CollectionDifferTest

open System
open Persimmon
open UseTestNameByReflection
open Foq
open FSharp.Object.Diff

type MockTarget = {
  IdentityStrategyResolver: Mock<IdentityStrategyResolver>
  ComparisonStrategyResolver: Mock<ComparisonStrategyResolver>
}

type Target = {
  Instances: Instances
  Differ: CollectionDiffer
}

let setup (f: MockTarget -> MockTarget) =
  let target =
    {
      IdentityStrategyResolver = Mock<IdentityStrategyResolver>()
      ComparisonStrategyResolver =
        Mock<ComparisonStrategyResolver>().SetupMethod(fun x -> <@ x.ResolveComparisonStrategy @>).Returns(null)
    }
    |> f
  let provider = DifferProvider()
  let dispatcher =
    DifferDispatcher(
      provider,
      Mock<CircularReferenceDetectorFactory>()
        .SetupMethod(fun x -> <@ x.CreateCircularReferenceDetector @>)
        .Returns(CircularReferenceDetector(DetectorReferenceMatchingMode.EqualityOperator))
        .Create(),
      Mock<CircularReferenceExceptionHandler>().Create(),
      Mock<IsIgnoredResolver>().Create(),
      Mock<IsReturnableResolver>().Create(),
      Mock<PropertyAccessExceptionHandlerResolver>().Create(),
      Mock<CategoryResolver>()
        .SetupMethod(fun x -> <@ x.ResolveCategories @>).Returns(Set.empty)
        .Create()
    )
  let differ =
    CollectionDiffer(
      dispatcher,
      target.ComparisonStrategyResolver.Create(),
      target.IdentityStrategyResolver.Create()
    )
  provider.PushAll(
    [
      differ
      PrimitiveDiffer(
        Mock<PrimitiveDefaultValueModeResolver>()
          .SetupMethod(fun x -> <@ x.ResolvePrimitiveDefaultValueMode @>)
          .Returns(Assigned).Create()
      )
    ])
  {
    Instances = Instances(RootAccessor, Seq.empty<string>, Seq.empty<string>, Seq.empty<string>)
    Differ = differ
  }

let ``accepts all collection types`` = parameterize {
  source [
    typeof<System.Collections.IEnumerable>
    typeof<Set<string>>
    typeof<ResizeArray<string>>
    typeof<string []>
    typeof<int list>
    typeof<int seq>
  ]
  run (fun t -> test {
    let target = setup id
    do! assertPred (target.Differ.Accepts(t))
  })
}

let ``returns untouched node when instances are same`` = test {
  let target = setup id
  let instances = Instances(RootAccessor, null, null, null)
  do! assertEquals Untouched (target.Differ.Compare(DiffNode.Root, instances).State)
}

let ``returns added node when instance has been added`` = test {
  let target = setup (fun target ->
    {
      target with
        IdentityStrategyResolver =
          target.IdentityStrategyResolver.SetupMethod(fun x -> <@ x.ResolveIdentityStrategy @>).Returns(EqualsIdentityStrategy)
      }
  )
  let instances = Instances(RootAccessor, seq [0], null, null)
  do! assertEquals Added (target.Differ.Compare(DiffNode.Root, instances).State)
}

let ``returns removed node when instance has been removed`` = test {
  let target = setup (fun target ->
    {
      target with
        IdentityStrategyResolver =
          target.IdentityStrategyResolver.SetupMethod(fun x -> <@ x.ResolveIdentityStrategy @>).Returns(EqualsIdentityStrategy)
      }
  )
  let instances = Instances(RootAccessor, null, seq [0], null)
  do! assertEquals Removed (target.Differ.Compare(DiffNode.Root, instances).State)
}

let ``compares via comparisonStrategy if available`` = test {
  let strategy = Mock<ComparisonStrategy>().Create()
  let target = setup (fun target ->
    {
      target with
        ComparisonStrategyResolver =
          Mock<ComparisonStrategyResolver>()
            .SetupMethod(fun x -> <@ x.ResolveComparisonStrategy @>)
            .Returns(strategy)
    }
  )
  Mock.Expect(<@ strategy.Compare(any(), target.Instances.Type, target.Instances.Working, target.Instances.Base) @>, once)
  return target.Differ.Compare(DiffNode.Root, target.Instances) |> ignore
}
