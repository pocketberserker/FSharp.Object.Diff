module FSharp.Object.Diff.Tests.NodePathTest

open System
open Persimmon
open UseTestNameByReflection
open FSharp.Object.Diff

let matches = parameterize {
  source [
    (NodePath.With("a"), NodePath.With("a"), true)
    (NodePath.With("a"), NodePath.With("z"), false)
  ]
  run (fun (path, givenPath, expected) -> test {
    do! assertEquals expected (path.Matches(givenPath))
  })
}

let isParentOf = parameterize {
  source [
    (NodePath.With("a"), NodePath.With("a", "b"), true)
    (NodePath.With("a", "b"), NodePath.With("a"), false)
    (NodePath.With("a"), NodePath.With("a"), false)
    (NodePath.With("a", "b"), NodePath.With("z", "b", "c"), false)
  ]
  run (fun (path, givenPath, expected) -> test {
    do! assertEquals expected (path.IsParentOf(givenPath))
  })
}

let isChildOf = parameterize {
  source [
    (NodePath.With("a", "b"), NodePath.With("a"), true)
    (NodePath.With("a"), NodePath.With("a", "b"), false)
    (NodePath.With("a"), NodePath.With("a"), false)
    (NodePath.With("a", "b"), NodePath.With("z", "b"), false)
  ]
  run (fun (path, otherPath, expected) -> test {
    do! assertEquals expected (path.IsChildOf(otherPath))
  })
}

let ``getLastElementSelector returns last element from path`` = parameterize {
  source [
    (NodePath.WithRoot(), RootElementSelector.Instance)
    (NodePath.With("a", "b", "c"), BeanPropertyElementSelector("c") :> ElementSelector)
  ]
  run (fun (path, expected) -> test {
    do! assertEquals expected path.LastElementSelector
  })
}

let equals = parameterize {
  source [
    (NodePath.WithRoot(), NodePath.WithRoot(), true)
    (NodePath.With("a"), NodePath.With("a"), true)
    (NodePath.With("a"), NodePath.With("a", "b"), false)
    (NodePath.With("a", "b"), NodePath.With("a"), false)
    (NodePath.With("a"), NodePath.With("b"), false)
  ]
  run (fun (path1, path2, result) -> test {
    do! assertEquals result (path1 = path2)
  })
}

let compare = parameterize {
  source [
    (NodePath.WithRoot(), NodePath.WithRoot(), 0)
    (NodePath.With("a"), NodePath.With("a"), 0)
    (NodePath.With("a"), NodePath.With("a", "b"), -1)
    (NodePath.With("a", "b"), NodePath.With("a"), 1)
    (NodePath.With("a"), NodePath.With("b"), 1)
  ]
  run (fun (path1, path2, result) -> test {
    do! assertEquals result (compare path1 path2)
  })
}

let ``startBuilding without elements`` = test {
  do! assertEquals [RootElementSelector.Instance] (NodePath.StartBuilding().Build().ElementSelectors)
}

let ``startBuilding with element`` = test {
  let elementSelector = CollectionItemElementSelector("foo")
  do!
    NodePath.StartBuilding().Element(elementSelector).Build().ElementSelectors
    |> assertEquals [RootElementSelector.Instance; elementSelector]
}

let ``startBuilding with propertyName`` = test {
  do!
    NodePath.StartBuilding().PropertyName("foo", "bar").Build().ElementSelectors
    |> assertEquals [RootElementSelector.Instance; BeanPropertyElementSelector("foo"); BeanPropertyElementSelector("bar")]
}

let ``startBuilding with mapKey`` = test {
  do!
    NodePath.StartBuilding().MapKey("foo").Build().ElementSelectors
    |> assertEquals [RootElementSelector.Instance; DictionaryKeyElementSelector("foo")]
}

let ``startBuilding with mapKey throws ArgumentException when key is null`` = test {
  let! ex = trap { it (NodePath.StartBuilding().MapKey(null).Build()) }
  do! assertEquals typeof<ArgumentException> (ex.GetType())
}

let ``startBuilding with collectionItem`` = test {
  do!
    NodePath.StartBuilding().CollectionItem("foo").Build().ElementSelectors
    |> assertEquals [RootElementSelector.Instance; CollectionItemElementSelector("foo")]
}

let startBuildingFrom = test {
  do!
    (NodePath.StartBuildingFrom(NodePath.With("foo")).Build().ElementSelectors)
    |> assertEquals [RootElementSelector.Instance; BeanPropertyElementSelector("foo")]
}

let ``startBuildingFrom: throws ArgumentException when key is null`` = test {
  let! ex = trap { it (NodePath.StartBuildingFrom(null).Build()) }
  do! assertEquals typeof<ArgumentException> (ex.GetType())
}

let ``withRoot: build path with only the root element`` = test {
  do! assertEquals [RootElementSelector.Instance] (NodePath.WithRoot().ElementSelectors)
}
