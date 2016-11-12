module FSharp.Object.Diff.Tests.CategoryServiceTest

open System
open Persimmon
open UseTestNameByReflection
open FSharp.Object.Diff
open Foq

type Alliance = Alliance

type Target = {
  CategoryService: CategoryService
  Accessor: PropertyAwareAccessor
  NodePath: NodePath
  NodeType: Type
  Node: DiffNode
  RootNode: DiffNode
}


let setup accessor =
  let nodeType = typeof<Alliance>
  let rootNode = DiffNode.NewRootNode()
  {
    CategoryService = CategoryService(ObjectDifferBuilder())
    Accessor = accessor
    NodePath = NodePath.With("foo")
    NodeType = nodeType
    RootNode = rootNode
    Node = DiffNode(rootNode, accessor, nodeType)
  }

let setupWithDefaultAccessor () =
  Mock<PropertyAwareAccessor>()
    .Setup(fun x -> <@ x.ElementSelector @>).Returns(BeanPropertyElementSelector("foo"))
    .Setup(fun x -> <@ x.CategoriesFromAttribute @>).Returns(Set.empty)
    .Create()
  |> setup

let ``resolveCategories: should return categories configured via path`` = test {
  let target = setupWithDefaultAccessor ()
  target.CategoryService.OfNode(target.NodePath).ToBe("Stark", "Lannister") |> ignore
  do! assertEquals (Set.ofList ["Stark"; "Lannister"]) (target.CategoryService.ResolveCategories(target.Node))
}

let ``resolveCategories: should return categories configured via type`` = test {
  let target = setupWithDefaultAccessor ()
  target.CategoryService.OfType(target.NodeType).ToBe("Stark", "Lannister") |> ignore
  do! assertEquals (Set.ofList ["Stark"; "Lannister"]) (target.CategoryService.ResolveCategories(target.Node))
}

let ``resolveCategories: should return categories configured via node`` = test {
  let target =
    Mock<PropertyAwareAccessor>()
      .Setup(fun x -> <@ x.ElementSelector @>).Returns(BeanPropertyElementSelector("foo"))
      .Setup(fun x -> <@ x.CategoriesFromAttribute @>).Returns(Set.ofList ["Stark"; "Lannister"])
      .Create()
    |> setup
  do! assertEquals (Set.ofList ["Stark"; "Lannister"]) (target.CategoryService.ResolveCategories(target.Node))
}

let ``resolveCategories: should return combined categories configured via all possible options`` = test {
  let target =
    Mock<PropertyAwareAccessor>()
      .Setup(fun x -> <@ x.ElementSelector @>).Returns(BeanPropertyElementSelector("foo"))
      .Setup(fun x -> <@ x.CategoriesFromAttribute @>).Returns(Set.ofList ["C"])
      .Create()
    |> setup
  target.CategoryService.OfNode(target.NodePath).ToBe("A") |> ignore
  target.CategoryService.OfType(target.NodeType).ToBe("B") |> ignore
  do! assertEquals (Set.ofList ["A"; "B"; "C"]) (target.CategoryService.ResolveCategories(target.Node)) 
}

let ``resolveCategories: should also return categories of parent nodes`` = test {
  let target =
    Mock<PropertyAwareAccessor>()
      .Setup(fun x -> <@ x.ElementSelector @>).Returns(BeanPropertyElementSelector("foo"))
      .Setup(fun x -> <@ x.CategoriesFromAttribute @>).Returns(Set.ofList ["B"])
      .Create()
    |> setup
  target.CategoryService.OfNode(NodePath.WithRoot()).ToBe("A") |> ignore
  do! assertEquals (Set.ofList ["A"; "B"]) (target.CategoryService.ResolveCategories(target.Node)) 
}

let ``resolveCategories: should return empty Set if no category is defined`` = test {
  do! assertEquals Set.empty ((CategoryService(ObjectDifferBuilder())).ResolveCategories(DiffNode.NewRootNode()) )
}
