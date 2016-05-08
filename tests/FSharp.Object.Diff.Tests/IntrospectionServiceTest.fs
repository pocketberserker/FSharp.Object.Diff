module FSharp.Object.Diff.Tests.IntrospectionServiceTest

open System
open Persimmon
open UseTestNameByReflection
open Foq
open FSharp.Object.Diff

let primitiveTypes = [
  typeof<char>
  typeof<sbyte>
  typeof<uint16>
  typeof<uint32>
  typeof<uint64>
  typeof<byte>
  typeof<int16>
  typeof<int>
  typeof<int64>
  typeof<float32>
  typeof<float>
]

type Target = {
  Service: IntrospectionService
  RootNode: DiffNode
}

let setup () =
  let service = IntrospectionService(ObjectDifferBuilder())
  service.SetDefaultIntrospector(Mock<Introspector>().Create()) |> ignore
  {
    Service = service
    RootNode = DiffNode.NewRootNodeWithType(typeof<ObjectWithString>)
  }

let ``introspection should always be disabled for #type`` = parameterize {
  source primitiveTypes
  run (fun t -> test {
    let target = setup ()
    let accessor =
      Mock<TypeAwareAccessor>()
        .Setup(fun x -> <@ x.Type @>).Returns(t)
        .Create()
    let childNode = DiffNode(target.RootNode, accessor, t)
    target.Service.OfType(t).ToBeEnabled() |> ignore
    do! assertPred (not <| target.Service.IsIntrospectable(childNode))
  })
}

let ``introspection should always be disabled for Arrays`` = parameterize {
  source [ typeof<int []>; typeof<string []>; typeof<obj []> ]
  run (fun t -> test {
    let target = setup ()
    let rootNode = DiffNode.NewRootNodeWithType(t)
    target.Service.OfType(t).ToBeEnabled() |> ignore
    do! assertPred (not <| target.Service.IsIntrospectable(rootNode))
  })
}

let ``introspection should always be disabled for nodes with unknown type (null)`` = test {
  let target = setup ()
  let rootNode = DiffNode.NewRootNode()
  target.Service.OfType(null).ToBeEnabled() |> ignore
  do! assertPred (not <| target.Service.IsIntrospectable(rootNode))
}

let ``introspection can be enabled via type`` = test {
  let target = setup ()
  target.Service.OfType(typeof<ObjectWithString>).ToBeEnabled() |> ignore
  do! assertPred (target.Service.IsIntrospectable(target.RootNode))
}

let ``introspection can be disabled via type`` = test {
  let target = setup ()
  target.Service.OfType(typeof<ObjectWithString>).ToBeDisabled() |> ignore
  do! assertPred (not <| target.Service.IsIntrospectable(target.RootNode))
}

let ``introspection can be re-enabled via type`` = test {
  let target = setup ()
  target.Service.OfType(typeof<ObjectWithString>).ToBeDisabled() |> ignore
  target.Service.OfType(typeof<ObjectWithString>).ToBeEnabled() |> ignore
  do! assertPred (target.Service.IsIntrospectable(target.RootNode))
}

let ``introspection can be enabled via node`` = test {
  let target = setup ()
  target.Service.OfNode(target.RootNode.Path).ToBeEnabled() |> ignore
  do! assertPred (target.Service.IsIntrospectable(target.RootNode))
}

let ``introspection can be disabled via node`` = test {
  let target = setup ()
  target.Service.OfNode(target.RootNode.Path).ToBeDisabled() |> ignore
  do! assertPred (not <| target.Service.IsIntrospectable(target.RootNode))
}

let ``introspection can be re-enabled via node`` = test {
  let target = setup ()
  target.Service.OfNode(target.RootNode.Path).ToBeDisabled() |> ignore
  target.Service.OfNode(target.RootNode.Path).ToBeEnabled() |> ignore
  do! assertPred (target.Service.IsIntrospectable(target.RootNode))
}

let ``node configuration overrules type configuration`` = test {
  let target = setup ()
  target.Service.OfNode(target.RootNode.Path).ToBeDisabled() |> ignore
  target.Service.OfType(typeof<ObjectWithString>).ToBeEnabled() |> ignore
  do! assertPred (not <| target.Service.IsIntrospectable(target.RootNode))
}

let ``attempting to set default introspector to null should cause ArgumentException`` = test {
  let target = setup ()
  let! ex = trap { it (target.Service.SetDefaultIntrospector(null)) }
  do! assertEquals typeof<ArgumentException> (ex.GetType())
}

let ``introspectorForNode returns default introspector if no special configuration exists`` = test {
  let target = setup ()
  let introspector = Mock<Introspector>().Create()
  target.Service.SetDefaultIntrospector(introspector) |> ignore
  do! assertEquals introspector (target.Service.IntrospectorForNode(target.RootNode))
}

let ``introspectorForNode returns introspector configured via type`` = test {
  let target = setup ()
  let introspector = Mock<Introspector>().Create()
  target.Service.OfType(typeof<string>).ToUse(introspector) |> ignore
  let rootNode = DiffNode.NewRootNode()
  rootNode.TypeInfo <- TypeInfo(typeof<string>)
  do! assertEquals introspector (target.Service.IntrospectorForNode(rootNode))
}

let ``introspectorForNode returns introspector configured via node path`` = test {
  let target = setup ()
  let introspector = Mock<Introspector>().Create()
  target.Service.OfNode(NodePath.WithRoot()).ToUse(introspector) |> ignore
  do! assertEquals introspector (target.Service.IntrospectorForNode(target.RootNode))
}
