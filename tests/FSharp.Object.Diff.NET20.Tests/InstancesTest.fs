module FSharp.Object.Diff.InstancesTest

open System
open System.Text
open System.Collections
open System.Collections.Generic
open Persimmon
open UseTestNameByReflection
open FSharp.Object.Diff
open Foq

let ``Type: returns obj type if base and working have different types`` = parameterize {
  source [
    (box <| StringBuilder(), box "")
    (box "foo", box 42)
  ]
  run (fun (base_, working) -> test {
    let instances = Instances(RootAccessor, working, base_, null)
    do! assertEquals typeof<obj> instances.Type
  })
}

let ``Type: returns '#resultTypeName' for base of type '#baseClassName' and working of type '#workingClassName'`` = parameterize {
  source [
    (box <| ResizeArray(), box <| ResizeArray(), typeof<ResizeArray<obj>>)
    (box <| ResizeArray(), box [], typeof<IEnumerable>)
    (box Map.empty<int, int>, box <| Dictionary<int, int>(), typeof<IDictionary<int, int>>)
  ]
  run (fun (base_, working, resultType) -> test {
    let instances = Instances.Of(working, base_)
    do! assertEquals resultType instances.Type
  })
}

let ``areNull: returns true when base and working are null`` = test {
  let instances = Instances(RootAccessor, null, null, null)
  do! assertPred instances.AreNull
}

let ``areNull: returns false when base is not null`` = test {
  let instances = Instances(RootAccessor, null, "", null)
  do! assertPred (not instances.AreNull)
}

let ``areNull: returns false when working is not null`` = test {
  let instances = Instances(RootAccessor, "", null, null)
  do! assertPred (not instances.AreNull)
}

let ``isPrimitiveType: returns true for primitive type (#type)`` = parameterize {
  source [
    typeof<bool>
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
    typeof<char>
  ]
  run (fun t -> test {
    let typeAwareAccessor =
      Mock<TypeAwareAccessor>()
        .Setup(fun x -> <@ x.Type @>).Returns(t)
        .Create()
    let instances = Instances(typeAwareAccessor, null, null, null)
    do! assertPred instances.IsPrimitiveType
  })
}

let ``isPrimitiveType: returns returns false for complex type`` = parameterize {
  source [
    typeof<obj>
    typeof<string>
    typeof<DateTime>
  ]
  run (fun t -> test {
    let typeAwareAccessor =
      Mock<TypeAwareAccessor>()
        .Setup(fun x -> <@ x.Type @>).Returns(t)
        .Create()
    let instances = Instances(typeAwareAccessor, null, null, null)
    do! assertPred (not instances.IsPrimitiveType)
  })
}

let typeAndDefault<'T> = (typeof<'T>, box Unchecked.defaultof<'T>)

let ``Fresh: returns default value for primitive types without custom initialization`` = parameterize {
  source [
    typeAndDefault<bool>
    typeAndDefault<sbyte>
    typeAndDefault<uint16>
    typeAndDefault<uint32>
    typeAndDefault<uint64>
    typeAndDefault<byte>
    typeAndDefault<int16>
    typeAndDefault<int>
    typeAndDefault<int64>
    typeAndDefault<float32>
    typeAndDefault<float>
    typeAndDefault<char>
  ]
  run (fun (t, d) -> test {
    let typeAwareAccessor =
      Mock<TypeAwareAccessor>()
        .Setup(fun x -> <@ x.Type @>).Returns(t)
        .Create()
    let instances = Instances(typeAwareAccessor, null, null, null)
    do! assertEquals d instances.Fresh
  })
}

let ``Fresh: returns fresh value from constructor for non-primitive types`` = test {
  let typeAwareAccessor =
    Mock<TypeAwareAccessor>()
      .Setup(fun x -> <@ x.Type @>).Returns(typeof<obj>)
      .Create()
  let instances = Instances(typeAwareAccessor, null, null, "foo")
  do! assertEquals (box "foo") instances.Fresh
}

let ``areSame: should return true when working and base are the same object`` = test {
  let o = obj()
  let instances = Instances(RootAccessor, o, o, null)
  do! assertPred instances.AreSame
}

let ``areSame: should return false when working and base are not the same object`` = test {
  let instances = Instances(RootAccessor, obj(), obj(), null)
  do! assertPred (not instances.AreSame)
}

let hasBeenAdded = parameterize {
  source [
    (null, null, null, null, false)
    (box "foo", null, null, null, true)
    (box "foo", box "foo", null, null, false)
    (null, box "foo", null, null, false)
    (box 2, box 0, box 0, typeof<int>, true)
    (box 2, box 1, box 1, typeof<int>, true)
    (box 1, box 1, box 1, typeof<int>, false)
    (box 2, box 0, box 1, typeof<int>, false)
  ]
  run (fun (working, base_, fresh, typeFromAccessor, result) -> test {
    let typeAwareAccessor =
      Mock<TypeAwareAccessor>()
        .Setup(fun x -> <@ x.Type @>).Returns(typeFromAccessor)
        .Create()
    let instances = Instances(typeAwareAccessor, working, base_, fresh)
    do! assertEquals result instances.HasBeenAdded
  })
}

let hasBeenRemoved = parameterize {
  source [
    (null, null, null, null, false)
    (box "foo", null, null, null, false)
    (box "foo", box "foo", null, null, false)
    (null, box "foo", null, null, true)
    (box 0, box 0, box 0, typeof<int>, false)
    (box 1, box 1, box 1, typeof<int>, false)
    (box 0, box 1, box 1, typeof<int>, false)
    (box 1, box 2, box 1, typeof<int>, true)
  ]
  run (fun (working, base_, fresh, typeFromAccessor, result) -> test {
    let typeAwareAccessor =
      Mock<TypeAwareAccessor>()
        .Setup(fun x -> <@ x.Type @>).Returns(typeFromAccessor)
        .Create()
    let instances = Instances(typeAwareAccessor, working, base_, fresh)
    do! assertEquals result instances.HasBeenRemoved
  })
}

let ``access: returns new instance created by using the given accessor`` = test {
  let instances = Instances(Mock<Accessor>().Create(), "working", "base", "fresh")
  let accessor =
    Mock<Accessor>.With(fun x ->
      <@
        x.Get("working") --> box "working2"
        x.Get("base") --> box "base2"
        x.Get("fresh") --> box "fresh2"
      @>)
  let accessedInstances = instances.Access(accessor)
  do! assertEquals (box "working2") accessedInstances.Working
  do! assertEquals (box "base2") accessedInstances.Base
  do! assertEquals (box "fresh2") accessedInstances.Fresh
  do! assertPred (Object.ReferenceEquals(accessedInstances.SourceAccessor, accessor))
}
