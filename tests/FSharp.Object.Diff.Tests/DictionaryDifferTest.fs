module FSharp.Object.Diff.Tests.DictionaryDifferTest

open System
open System.Collections.Generic
open Persimmon
open UseTestNameByReflection
open Foq
open FSharp.Object.Diff

let setup (f: Mock<ComparisonStrategyResolver> -> Mock<ComparisonStrategyResolver>) =
  let target =
    Mock<ComparisonStrategyResolver>().SetupMethod(fun x -> <@ x.ResolveComparisonStrategy @>).Returns(null)
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
  let differ = DictionaryDiffer(dispatcher, target.Create())
  provider.PushAll(
    [
      differ
      PrimitiveDiffer(
        Mock<PrimitiveDefaultValueModeResolver>()
          .SetupMethod(fun x -> <@ x.ResolvePrimitiveDefaultValueMode @>)
          .Returns(Assigned).Create()
      )
    ])
  differ

let ``accepts all dictionary types`` = parameterize {
  source [
    (typeof<System.Collections.IDictionary>, true)
    (typeof<Dictionary<int, string>>, true)
    (typeof<IDictionary<int, string>>, true)
    (typeof<Map<int, int>>, true)
    (typeof<string>, false)
  ]
  run (fun (t, accept) -> test {
    let target = setup id
    do! assertEquals accept (target.Accepts(t))
  })
}

let ``mark node as added when map was null and has been changed to an instance`` = test {
  let target = setup id
  let instances = Instances(RootAccessor, dict [(0, 0)], null, null)
  do! assertEquals Added (target.Compare(DiffNode.Root, instances).State)
}

let ``returns removed node when instance has been removed`` = test {
  let target = setup id
  let instances = Instances(RootAccessor, null, dict [(0, 0)], null)
  do! assertEquals Removed (target.Compare(DiffNode.Root, instances).State)
}

let ``compares via comparisonStrategy if available`` = test {
  let strategy = Mock<ComparisonStrategy>().Create()
  let target = setup (fun target ->
    Mock<ComparisonStrategyResolver>()
      .SetupMethod(fun x -> <@ x.ResolveComparisonStrategy @>)
      .Returns(strategy)
  )
  let instances = Instances(RootAccessor, dict [(0, 0)], dict [(0, 0)], null)
  Mock.Expect(<@ strategy.Compare(any(), instances.Type, instances.Working, instances.Base) @>, once)
  return target.Compare(DiffNode.Root, instances) |> ignore
}
