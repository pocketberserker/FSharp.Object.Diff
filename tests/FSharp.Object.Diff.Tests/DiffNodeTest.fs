module FSharp.Object.Diff.Tests.DiffNodeTest

open System
open Persimmon
open UseTestNameByReflection
open FSharp.Object.Diff
open Foq

let ``getChild: always starts at root node when called with NodePath`` = test {
  let rootNode = DiffNode.NewRootNode()
  let childNodeA = DiffNode(null, CollectionItemAccessor("A"), null)
  let childNodeB = DiffNode(null, CollectionItemAccessor("B"), null)
  rootNode.AddChild(childNodeA)
  childNodeA.AddChild(childNodeB)
  do! childNodeA.Child(NodePath.StartBuilding().CollectionItem("A").CollectionItem("B").Build())
    |> assertEquals childNodeB
}

let ``hasChanges: returns #expectedHasChangesResult when node state is #state`` = parameterize {
  source [
    (Added, true)
    (Removed, true)
    (Changed, true)
    (Untouched, false)
    (Ignored, false)
    (Circular, false)
  ]
  run (fun (state, expectedHasChangesResult) -> test {
    let node = DiffNode.NewRootNode()
    node.State <- state
    do! assertEquals expectedHasChangesResult node.HasChanges
  })
}

let ``getPropertyPath: returns absolute path for root node`` = test {
  do! assertEquals (NodePath.WithRoot()) (DiffNode.NewRootNode().Path)
}

let ``addChild: fails with exception when attempting to add root node`` = test {
  let rootNode = DiffNode.NewRootNode()
  let anotherRootNode = DiffNode.NewRootNode()
  let! ex = trap { it (rootNode.AddChild(anotherRootNode)) }
  do! assertEquals typeof<ArgumentException> (ex.GetType())
}

let ``addChild: fails with exception when attempting to add node that is already child of another node`` = test {
  let childNode = DiffNode(DiffNode.NewRootNode(), Mock<Accessor>().Create(), typeof<obj>)
  let! ex = trap { it (DiffNode.NewRootNode().AddChild(childNode)) }
  do! assertEquals typeof<ArgumentException> (ex.GetType())
}

let ``addChild: fails with exception when attempting to add node to itself`` = test {
  let node = DiffNode.NewRootNode()
  let! ex = trap { it (node.AddChild(node)) }
  do! assertEquals typeof<ArgumentException> (ex.GetType())
}

let ``addChild: establishes parent-child relationship`` = test {
  let parent = DiffNode.NewRootNode()
  let childAccessor =
    Mock<Accessor>()
      .Setup(fun x -> <@ x.ElementSelector @>).Returns(BeanPropertyElementSelector("foo"))
      .Create()
  let child = new DiffNode(null, childAccessor, typeof<string>)
  parent.AddChild(child)
  do! assertPred (Object.ReferenceEquals(parent.Child(child.ElementSelector), child))
  do! assertPred (Object.ReferenceEquals(child.ParentNode, parent))
}

let ``addChild: changes parent node state to CHANGED if child node has changes`` = test {
  let parentNode = DiffNode.NewRootNode()
  let childNode = DiffNode(parentNode, Mock<Accessor>().Setup(fun x -> <@ x.ElementSelector @>).Returns(Mock<ElementSelector>().Create()).Create(), typeof<string>)
  childNode.State <- Changed
  do! assertEquals Untouched parentNode.State
  do! assertPred childNode.HasChanges
  parentNode.AddChild(childNode)
  do! assertEquals Changed parentNode.State
}

let ``PropertyAttributes: delegates to accessor if it is property aware`` = test {
  let attribute = ObjectDiffTestAttribute() :> Attribute
  let accessor =
    Mock<PropertyAwareAccessor>()
      .Setup(fun x -> <@ x.PropertyAttributes @>).Returns([attribute])
      .Create()
  let node = DiffNode(null, accessor, typeof<obj>)
  do! assertEquals 1 (Seq.length node.PropertyAttributes)
  do! assertPred (node.PropertyAttributes |> Seq.exists ((=) attribute))
}

let ``PropertyAttributes: returns empty set if accessor is not property aware`` = test {
  let node = DiffNode(null, Mock<Accessor>().Create(), typeof<obj>)
  do! assertPred (Seq.isEmpty node.PropertyAttributes)
}

let ``GetPropertyAttribute: should delegate call to property accessor`` = test {
  let attribute = ObjectDiffTestAttribute()
  let accessor =
    Mock<PropertyAwareAccessor>()
      .SetupMethod(fun x -> <@ x.GetPropertyAttribute @>).Returns(attribute)
      .Create()
  let node = DiffNode(DiffNode.NewRootNode(), accessor)
  do! assertEquals attribute (node.GetPropertyAttribute<ObjectDiffTestAttribute>())
}

let ``GetPropertyAttribute: should return null if accessor is not a property aware accessor`` = test {
  let accessor = Mock<Accessor>().Create()
  let node = DiffNode(null, accessor, typeof<obj>)
  do! assertEquals null (node.GetPropertyAttribute<ObjectDiffTestAttribute>())
}

let ``PropertyName: returns name from PropertyAwareAccessor`` = test {
  let expectedPropertyName = "foo"
  let nodeWithPropertyName = DiffNode(null, Mock<PropertyAwareAccessor>().Setup(fun x -> <@ x.PropertyName @>).Returns(expectedPropertyName).Create(), typeof<obj>)
  do! assertEquals expectedPropertyName (nodeWithPropertyName.PropertyName)
}

let ``PropertyName: returns name of parent node if it doesn't have one itself`` = test {
  let expectedPropertyName = "foo"
  let parentNodeWithPropertyName =
    DiffNode(null, Mock<PropertyAwareAccessor>().Setup(fun x -> <@ x.PropertyName @>).Returns(expectedPropertyName).Create(), typeof<obj>)
  let nodeWithoutPropertyName = DiffNode(parentNodeWithPropertyName, Mock<Accessor>().Create(), typeof<obj>)
  do! assertEquals expectedPropertyName (nodeWithoutPropertyName.PropertyName)
}

let ``PropertyName: returns null when property name can not be resolved from accessor`` = test {
  let node = DiffNode(null, Mock<Accessor>().Create(), typeof<obj>)
  do! assertEquals null node.PropertyName
}

let ``isPropertyAware: returns #expectedResult when acessor #doesOrDoesNotImplement PropertyAwareAccessor interface`` = parameterize {
  source [
    (Mock<Accessor>().Create(), false)
    (Mock<PropertyAwareAccessor>().Create() :> Accessor, true)
  ]
  run (fun (accessor, expectedResult) -> test {
    let node = DiffNode(null, accessor, typeof<obj>)
    do! assertEquals expectedResult node.IsProperyAware
  })
}

let ``should return added categories`` = test {
  let node = DiffNode(null, Mock<Accessor>().Create(), typeof<obj>)
  node.AddCategories(["addedCategory"])
  do! assertEquals (Set.ofList ["addedCategory"]) node.Categories
}
