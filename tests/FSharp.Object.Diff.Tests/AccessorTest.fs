namespace FSharp.Object.Diff.Tests

open Persimmon
open UseTestNameByReflection
open System
open System.Collections.Generic
open FSharp.Object.Diff

module RootAccessorTest =

  let accessor = RootAccessor :> Accessor

  let ``get: returns target object`` = test {
    let target = obj()
    do! assertEquals target (accessor.Get(target))
  }

  let ``set: fails with UnsupportedOperationException`` = test {
    let target = obj()
    let! e = trap { it (accessor.Set(target, "anything")) }
    do! assertEquals typeof<NotSupportedException> (e.GetType())
  }

  let ``unset: fails with UnsupportedOperationException`` = test {
    let target = obj()
    let! e = trap { it (accessor.Unset(target)) }
    do! assertEquals typeof<NotSupportedException> (e.GetType())
  }

module CollectionItemAccessorTest =

  let anyReferenceItem = obj()

  let ``get: should return item equal to reference item from collection`` = test {
    let accessor = CollectionItemAccessor(ObjectWithIdentityAndValue.idOnly "1")
    let collection = ResizeArray<obj>()
    collection.Add({ Id = "1"; Value = "foo" })
    let item = accessor.Get(collection) :?> ObjectWithIdentityAndValue
    do! assertEquals "1" item.Id
    do! assertEquals "foo" item.Value
  }

  let ``get: should return null when no item in the collection matches the reference item`` = test {
    let referenceItemForNonExistingItem = ObjectWithIdentityAndValue.idOnly "1"
    let accessor = CollectionItemAccessor(referenceItemForNonExistingItem)
    do! assertEquals null (accessor.Get(ResizeArray<obj>()))
  }

  let ``get: should return null when reference item is null`` = test {
    let accessor = CollectionItemAccessor(null)
    let collection = ResizeArray<obj>()
    collection.Add("some-item,")
    do! assertEquals null (accessor.Get(collection))
  }

  let ``get: should fail with exception if target object is not a collection`` = test {
    let accessor = CollectionItemAccessor(anyReferenceItem)
    let notCollection = obj()
    let! e = trap { it (accessor.Get(notCollection)) }
    do! assertEquals typeof<ArgumentException> (e.GetType())
  }

  let ``set: should insert non-existing item into collection`` = test {
    let collection = ResizeArray<obj>()
    let accessor = CollectionItemAccessor(ObjectWithIdentityAndValue.idOnly "foo")
    accessor.Set(collection, { Id = "foo"; Value = "bar" })
    do!
      collection
      |> Seq.map (unbox<ObjectWithIdentityAndValue>)
      |> Seq.tryFind (fun x -> x.Id = "foo" && x.Value = "bar")
      |> assertNotEquals None
  }

  let ``getType: should return type of reference item`` = parameterize {
    source [
      (null, null)
      (box "foo", typeof<string>)
      (box DateTime.Now, typeof<DateTime>)
    ]
    run (fun (referenceItem, expectedType) -> test {
      let accessor = CollectionItemAccessor(referenceItem)
      do! assertEquals expectedType accessor.Type
    })
  }

  let ``getElementSelector: should return proper element selector`` = test {
    let referenceItem = "foo"
    let accessor = CollectionItemAccessor(referenceItem)
    do! assertEquals (CollectionItemElementSelector(referenceItem) :> ElementSelector) accessor.ElementSelector
  }

module MapEntryAccessorTest =

  let accessor = MapEntryAccessor("b")
  let key1 = { Id = "key"; Value = "1" }
  let key2 = { Id = "key"; Value = "2" }

  let ``provide access to its path element`` = test {
    do! assertPred (accessor.ElementSelector :? MapKeyElementSelector)
  }

  let ``provide write access to referenced value in any map`` = test {
    let dict = Dictionary<obj, obj>()
    accessor.Set(dict, "foo")
    do! assertEquals (box "foo") (dict.["b"])
    accessor.Set(dict, "bar")
    do! assertEquals (box "bar") (dict.["b"])
  }

  let ``provide read access to referenced value in any map`` = test {
    let dict = Dictionary<obj, obj>()
    dict.Add("b", "foo")
    do! assertEquals (box "foo") (accessor.Get(dict))
  }

  let ``throw exception when trying to read from non map object`` = test {
    let! e = trap { it (accessor.Get(obj())) }
    do! assertEquals typeof<ArgumentException> (e.GetType())
  }

  let ``return null when reading from null object`` = test {
    do! assertEquals null (accessor.Get(null))
  }

  let ``remove referenced entry from any map`` = test {
    let dict = Dictionary<obj, obj>()
    dict.Add("b", "foo")
    accessor.Unset(dict)
    do! assertPred (Seq.isEmpty dict)
  }

  let ``return the key object of the given map`` = parameterize {
    source [
      (key1, key2)
      (key1, key1)
    ]
    run (fun (referenceKey, actualMapKey) -> test {
      let accessor = MapEntryAccessor(referenceKey)
      let dict = Dictionary<obj, obj>()
      dict.Add(actualMapKey, "foo")
      do! assertEquals (box actualMapKey) (accessor.GetKey(dict))
    })
  }

  let ``return null as the key object if the target object is null`` = test {
    do! assertEquals null (accessor.GetKey(null))
  }

  let ``return null as the key object if the given map does not contain it`` = test {
    let dict = Dictionary<obj, obj>()
    dict.Add("d", "whatever value")
    do! assertEquals null (accessor.GetKey(dict))
  }
