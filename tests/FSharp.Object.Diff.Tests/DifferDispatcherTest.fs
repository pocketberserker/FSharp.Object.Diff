module FSharp.Object.Diff.Tests.DifferDispatcherTest

open System
open Persimmon
open UseTestNameByReflection
open Foq
open FSharp.Object.Diff

type MockTarget = {
  Factory: Mock<CircularReferenceDetectorFactory>
  Handler: Mock<CircularReferenceExceptionHandler>
  IgnoredResolver: Mock<IsIgnoredResolver>
  CategoryResolver: Mock<CategoryResolver>
  ReturnableResolver: Mock<IsReturnableResolver>
  HandlerResolver: Mock<PropertyAccessExceptionHandlerResolver>
}

type Target = {
  Handler: CircularReferenceExceptionHandler
  Dispatcher: DifferDispatcher
  HandlerResolver: PropertyAccessExceptionHandlerResolver
}

let setup provider (detector: CircularReferenceDetector) (f: MockTarget -> MockTarget) =
  let target =
    {
      Factory = Mock<CircularReferenceDetectorFactory>()
        .SetupMethod(fun x -> <@ x.CreateCircularReferenceDetector @>).Returns(detector)
      Handler = Mock<CircularReferenceExceptionHandler>()
      IgnoredResolver = Mock<IsIgnoredResolver>()
      CategoryResolver = Mock<CategoryResolver>()
        .SetupMethod(fun x -> <@ x.ResolveCategories @>).Returns(Set.empty)
      ReturnableResolver = Mock<IsReturnableResolver>()
      HandlerResolver = Mock<PropertyAccessExceptionHandlerResolver>()
    }
    |> f
  let factory = target.Factory.Create()
  let handler = target.Handler.Create()
  let ignoredResolver = target.IgnoredResolver.Create()
  let categoryResolver = target.CategoryResolver.Create()
  let returnableResolver = target.ReturnableResolver.Create()
  let handlerResolver = target.HandlerResolver.Create()
  {
    Handler = handler
    Dispatcher =
      DifferDispatcher(provider, factory, handler, ignoredResolver, returnableResolver, handlerResolver, categoryResolver)
    HandlerResolver = handlerResolver
  }

let provider = DifferProvider()

let ``when circular reference is detected the node should be marked as circular`` = test {
  let detector = { new CircularReferenceDetector(DetectorReferenceMatchingMode.EqualityOperator) with
    override __.Push(_, nodePath) = raise <| CircularReferenceException nodePath
  }
  let target = setup provider detector id
  let node = target.Dispatcher.Dispatch(DiffNode.Root, Instances.Of("*", "*"), RootAccessor)
  do! assertEquals Circular node.State
}

let ``when circular reference is detected the node should hold the path to the node it circles back to`` = test {
  let detector = { new CircularReferenceDetector(DetectorReferenceMatchingMode.EqualityOperator) with
    override __.Push(_, nodePath) = raise <| CircularReferenceException nodePath
  }
  let target = setup provider detector id
  Mock.Expect(<@ target.Handler.OnCircularReferenceException(any()) @>, once)
  let node = target.Dispatcher.Dispatch(DiffNode.Root, Instances.Of("*", "*"), RootAccessor)
  do! assertEquals (NodePath.WithRoot()) node.CircleStartPath
}

let ``when circular reference is detected the node should be passed to the exception handler before it is returned`` = test {
  let handledNode: DiffNode ref = ref null
  let detector = { new CircularReferenceDetector(DetectorReferenceMatchingMode.EqualityOperator) with
    override __.Push(_, nodePath) = raise <| CircularReferenceException nodePath
  }
  let target =
    setup provider detector (fun target ->
      {
        target with
          Handler =
            target.Handler.Setup(fun x -> <@ x.OnCircularReferenceException(any()) @>).Calls<DiffNode>(fun n -> handledNode := n)
      }
    )
  Mock.Expect(<@ target.Handler.OnCircularReferenceException(any()) @>, once)
  let node = target.Dispatcher.Dispatch(DiffNode.Root, Instances.Of("*", "*"), RootAccessor)
  do! assertPred (Object.ReferenceEquals(node, !handledNode))
}

let ``throw exception if no differ can be found for instance type`` = test {
  let provider = { new DifferProvider() with
    override __.RetrieveDifferForType(_) = null
  }
  let detector = { new CircularReferenceDetector(DetectorReferenceMatchingMode.EqualityOperator) with
    override __.Push(_, _) = ()
  }
  let target = setup provider detector id
  let! ex = trap { it (target.Dispatcher.Dispatch(DiffNode.Root, Instances.Of("*", "*"), RootAccessor)) }
  do! assertEquals typeof<InvalidOperationException> (ex.GetType())
}

let ``should delegate property read exception to PropertyAccessExceptionHandler`` = test {
  let propertyExceptionHandler = Mock.Of<PropertyAccessExceptionHandler>()
  let propertyAccessor =
    Mock<PropertyAwareAccessor>()
      .Setup(fun x -> <@ x.PropertyName @>).Returns("foo")
      .Create()
  let propertyAccessException = PropertyReadException("foo", typeof<DateTime>, exn())
  let instances = { new Instances(propertyAccessor, null, obj(), obj()) with
    override __.Access(_) = raise propertyAccessException
    override __.Type = typeof<DateTime>
  }
  let detector = CircularReferenceDetector(DetectorReferenceMatchingMode.EqualityOperator)
  let target = setup provider detector id
  Mock.Expect(<@ target.HandlerResolver.ResolvePropertyAccessExceptionHandler(typeof<DateTime>, "foo") @>, once)
  Mock.Expect(<@ propertyExceptionHandler.OnPropertyReadException(propertyAccessException, any()) @>, once)
  return target.Dispatcher.Dispatch(DiffNode.Root, instances, propertyAccessor)
}

let ``when reading a property value caused an exception the returned node should have state INACESSIBLE`` = test {
  let propertyAccessor = Mock<PropertyAwareAccessor>().Create()
  let instances = { new Instances(propertyAccessor, null, obj(), obj()) with
    override __.Access(_) = raise <| PropertyReadException("foo", typeof<DateTime>, exn())
  }
  let detector = CircularReferenceDetector(DetectorReferenceMatchingMode.EqualityOperator)
  let target = setup provider detector id
  let node = target.Dispatcher.Dispatch(DiffNode.Root, instances, propertyAccessor)
  do! assertEquals Inaccessible node.State
}
