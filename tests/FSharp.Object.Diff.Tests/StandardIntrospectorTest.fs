module FSharp.Object.Diff.Tests.StandardIntrospectorTest

open System
open Persimmon
open UseTestNameByReflection
open FSharp.Object.Diff

let introspector = StandardIntrospector :> Introspector

let introspect (typ: Type) =
  introspector.Introspect(typ).Accessors
  |> List.fold (fun m a -> Map.add a.PropertyName a m) Map.empty

type TypeWithOnlyOneProperty = {
  mutable Value: obj
}

type TypeWithTwoProperties = {
  Foo: obj
  Bar: obj
}

let ``should return proper accessor for property`` = test {
  let accessor = introspect typeof<TypeWithOnlyOneProperty> |> Map.find "Value"
  do! assertEquals "Value" accessor.PropertyName
  let target = { Value = null }
  do! assertEquals null (accessor.Get(target))
  accessor.Set(target, "bar")
  do! assertEquals (box "bar") (accessor.Get(target))
  do! assertPred (Set.isEmpty accessor.CategoriesFromAttribute)
}

let ``should return PropertyAwareAccessors for each property of the given class`` = test {
  let accessors = introspect typeof<TypeWithTwoProperties>
  do! assertEquals 2 (accessors |> Map.toSeq |> Seq.length)
  do! assertNotEquals None (accessors |> Map.tryFind "Foo")
  do! assertNotEquals None (accessors |> Map.tryFind "Bar")
}

type TypeWithAttribute = {
  [<ObjectDiffProperty(Categories = [|"category1"; "category2"|])>]
  Value: string
}

let ``should apply categories of ObjectDiffProperty annotation to accessor`` = test {
  let accessor = introspect typeof<TypeWithAttribute> |> Map.find "Value"
  do! assertEquals 2 (Set.count accessor.CategoriesFromAttribute)
  do! assertEquals (Set.ofList ["category1"; "category2"]) accessor.CategoriesFromAttribute
}

let ``should throw exception when invoked without type`` = test {
  let! ex = trap { it (introspector.Introspect(null)) }
  do! assertEquals typeof<ArgumentException> (ex.GetType())
}

type TypeWithNothingButDefaultProperties = class end

let ``should skip default class properties`` = test {
  do! assertPred (introspect typeof<TypeWithNothingButDefaultProperties> |> Map.isEmpty)
}

type TypeWithPropertyWithoutGetter () =
  let mutable value: string = null
  member __.Value with set(v) = value <- v

let ``should skip properties without getter`` = test {
  do! assertPred (introspect typeof<TypeWithPropertyWithoutGetter> |> Map.isEmpty)
}
