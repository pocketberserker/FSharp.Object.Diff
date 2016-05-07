module FSharp.Object.Diff.Tests.BeanDifferTest

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
  Differ: BeanDiffer
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
      BeanDiffer(
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
    typeof<obj>
    typeof<DateTime>
  ]
  run (fun t -> test {
    let target = setup id
    do! assertPred (target.Differ.Accepts(t))
  })
}

let ``rejects all primitive types`` = parameterize {
  source [
    typeof<int>
    typeof<int []>
    typeof<bool>
  ]
  run (fun t -> test {
    let target = setup id
    do! assertPred (not <| target.Differ.Accepts(t))
  })
}

let ``returns untouched node if working and base are null`` = test {
  let target = setup id
  let instances = Instances(RootAccessor, null, null, null)
  do! assertEquals Untouched (target.Differ.Compare(DiffNode.Root, instances).State)
}

let ``returns added node if working is not null and base is`` = test {
  let target = setup id
  let instances = Instances(RootAccessor, "any", null, null)
  do! assertEquals Added (target.Differ.Compare(DiffNode.Root, instances).State)
}

let ``returns removed node if working is null and base is not`` = test {
  let target = setup id
  let instances = Instances(RootAccessor, null, "any", null)
  do! assertEquals Removed (target.Differ.Compare(DiffNode.Root, instances).State)
}

let ``returns untouched node if working and base are the same instance`` = test {
  let target = setup id
  let o = obj()
  let instances = Instances(RootAccessor, o, o, null)
  do! assertEquals Untouched (target.Differ.Compare(DiffNode.Root, instances).State)
}

let ``compares via comparisonStrategy if one exists`` = test {
  let target = setup (fun target ->
    {
      target with
        ComparisonStrategyResolver =
          target.ComparisonStrategyResolver
            .SetupMethod(fun x -> <@ x.ResolveComparisonStrategy @>)
            .Returns(target.ComparisonStrategy)
    }
  )
  Mock.Expect(<@ target.ComparisonStrategy.Compare(any(), target.Instances.Type, target.Instances.Working, target.Instances.Base) @>, once)
  return target.Differ.Compare(DiffNode.Root, target.Instances) |> ignore
}

let ``compares via introspection if object is introspectable`` = test {
  let target = setup (fun target ->
    {
      target with
        IntrospectableResolver =
          target.IntrospectableResolver.SetupMethod(fun x -> <@ x.IsIntrospectable @>).Returns(true)
        TypeInfoResolver =
          target.TypeInfoResolver.SetupMethod(fun x -> <@ x.TypeInfoForNode @>).Returns(TypeInfo(typeof<obj>))
    }
  )
  Mock.Expect(<@ target.TypeInfoResolver.TypeInfoForNode(any()) @>, once)
  return target.Differ.Compare(DiffNode.Root, target.Instances) |> ignore
}

let ``delegate comparison of properties to DifferDispatcher when comparing via introspector`` = test {
  let accessor =
    Mock<PropertyAwareAccessor>()
      .Setup(fun x -> <@ x.ElementSelector @>).Returns(BeanPropertyElementSelector("Example"))
      .Create()
  let propertyNode = DiffNode(DiffNode.Root, accessor)
  let target = setup (fun target ->
    let typeInfo = TypeInfo(typeof<obj>)
    typeInfo.AddPropertyAccessor(accessor)
    {
      target with
        IntrospectableResolver =
          target.IntrospectableResolver
            .Setup(fun x -> <@ x.IsIntrospectable(is(fun node -> node.IsRootNode)) @>).Returns(true)
        TypeInfoResolver =
          target.TypeInfoResolver.SetupMethod(fun x -> <@ x.TypeInfoForNode @>).Returns(typeInfo)
        ReturnableResolver =
          target.ReturnableResolver.Setup(fun x -> <@ x.IsReturnable(propertyNode) @>).Returns(true)
    }
  )
  let rootNode = target.Differ.Compare(DiffNode.Root, target.Instances)
  do! assertEquals 1 rootNode.ChildCount
  do! assertEquals propertyNode (rootNode.Child(propertyNode.ElementSelector))
}

let ``add property nodes only if they are returnable`` = test {
  let accessor =
    Mock<PropertyAwareAccessor>()
      .Setup(fun x -> <@ x.ElementSelector @>).Returns(BeanPropertyElementSelector("Example"))
      .Create()
  let propertyNode = DiffNode(DiffNode.Root, accessor)
  let target = setup (fun target ->
    let typeInfo = TypeInfo(typeof<obj>)
    typeInfo.AddPropertyAccessor(accessor)
    {
      target with
        IntrospectableResolver =
          target.IntrospectableResolver
            .Setup(fun x -> <@ x.IsIntrospectable(is(fun node -> node.IsRootNode)) @>).Returns(true)
        TypeInfoResolver =
          target.TypeInfoResolver.SetupMethod(fun x -> <@ x.TypeInfoForNode @>).Returns(typeInfo)
        ReturnableResolver =
          target.ReturnableResolver.Setup(fun x -> <@ x.IsReturnable(propertyNode) @>).Returns(false)
    }
  )
  let rootNode = target.Differ.Compare(DiffNode.Root, target.Instances)
  do! assertPred (not <| rootNode.HasChildren)
}

let ``assigns type info resolved via type info resolver to bean node when comparing via introspection`` = test {
  let accessor =
    Mock<PropertyAwareAccessor>()
      .Setup(fun x -> <@ x.ElementSelector @>).Returns(BeanPropertyElementSelector("Example"))
      .Create()
  let propertyNode = DiffNode(DiffNode.Root, accessor)
  let typeInfo = TypeInfo(typeof<obj>)
  let target = setup (fun target ->
    typeInfo.AddPropertyAccessor(accessor)
    {
      target with
        IntrospectableResolver =
          target.IntrospectableResolver
            .Setup(fun x -> <@ x.IsIntrospectable(is(fun node -> node.IsRootNode)) @>).Returns(true)
        TypeInfoResolver =
          target.TypeInfoResolver.Setup(fun x -> <@ x.TypeInfoForNode(is(fun node -> node.IsRootNode)) @>).Returns(typeInfo)
        ReturnableResolver =
          target.ReturnableResolver.Setup(fun x -> <@ x.IsReturnable(propertyNode) @>).Returns(false)
    }
  )
  let rootNode = target.Differ.Compare(DiffNode.Root, target.Instances)
  do! assertEquals typeInfo rootNode.TypeInfo
}
