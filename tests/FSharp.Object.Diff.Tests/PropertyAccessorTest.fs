module FSharp.Object.Diff.Tests.PropertyAccessorTest

open System
open Persimmon
open UseTestNameByReflection
open FSharp.Object.Diff

type Target = {
  PropertyAccessor: PropertyAccessor
  Item: ObjectWithHashCodeAndEquals
}

let setup () ={
  PropertyAccessor =
    PropertyAccessor(typeof<ObjectWithHashCodeAndEquals>.GetProperty("Value"))
  Item = { Key = "foo"; Value = null; Item = None }
}

let ``assign new value if property is writable`` = test {
  let target = setup ()
  do! assertEquals null target.Item.Value
  target.PropertyAccessor.Set(target.Item, "bar")
  do! assertEquals "bar" target.Item.Value
}

let ``assign nothing if no write method is available`` = test {
  let target =
    {
      setup () with
        PropertyAccessor = PropertyAccessor(typeof<ObjectWithHashCodeAndEquals>.GetProperty("Key"))
    }
  target.PropertyAccessor.Set(target.Item, "bar")
  do! assertEquals null target.Item.Value
}

let ``retrieve correct value`` = test {
  let target = setup ()
  target.Item.Value <- "bar"
  do! target.PropertyAccessor.Get(target.Item)
    |> unbox<string>
    |> assertEquals "bar"
}

let ``retrieve null if target is null`` = test {
  let target = setup ()
  do! assertEquals null (target.PropertyAccessor.Get(null))
}

let ``assign nothing if target is null`` = test {
  let target = setup ()
  target.PropertyAccessor.Set(null, "bar")
  return ()
}

let ``unset property value`` = test {
  let target = setup ()
  target.Item.Value <- "bar"
  target.PropertyAccessor.Unset(target.Item)
  do! assertEquals null target.Item.Value
}

let ``return property value type`` = test {
  let target = setup ()
  do! assertEquals typeof<string> target.PropertyAccessor.Type
}

let ``return proper path element`` = test {
  let target = setup ()
  do! assertEquals (BeanPropertyElementSelector("Value") :> ElementSelector) target.PropertyAccessor.ElementSelector
}

let ``returns attributes of property getter`` = test {
  let typ = typeof<ObjectWithAnnotatedProperty>
  let property = typ.GetProperty("Value")
  let accessor = PropertyAccessor(property)
  let expectedAttributes =
    Attribute.GetCustomAttributes(property)
    |> Array.filter (fun x -> x :? ObjectDiffPropertyAttribute || x :? ObjectDiffTestAttribute)
  do! assertEquals 2 (Seq.length expectedAttributes)
  do! assertPred (accessor.ReadMethodAttributes |> Seq.exists (fun x -> expectedAttributes |> Array.exists ((=) x)))
}

let ``returns specific attribute of property getter`` = test {
  let typ = typeof<ObjectWithAnnotatedProperty>
  let property = typ.GetProperty("Value")
  let accessor = PropertyAccessor(property)
  let expectedAttribute =
    Attribute.GetCustomAttributes(property)
    |> Array.find (fun x -> x :? ObjectDiffPropertyAttribute)
    :?> ObjectDiffPropertyAttribute
  do! assertEquals expectedAttribute (accessor.GetReadMethodAttribute<ObjectDiffPropertyAttribute>())
}

type AnnotatedType = {
  [<ObjectDiffProperty(Categories = [|"A"; "B"; "C"|])>]
  Value: string
  UnannotatedValue: string
}

let ``CategoriesFromAttribute returns empty set when attribute is absent`` = test {
  let accessor = PropertyAccessor(typeof<AnnotatedType>.GetProperty("UnannotatedValue"))
  do! assertEquals Set.empty accessor.CategoriesFromAttribute
}

let ``CategoriesFromAttribute returns set of categories from attribute`` = test {
  let accessor = PropertyAccessor(typeof<AnnotatedType>.GetProperty("Value"))
  do! assertEquals (Set.ofList ["A"; "B"; "C"]) accessor.CategoriesFromAttribute
}
