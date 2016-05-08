module FSharp.Object.Diff.Tests.CircularReferenceDetectorTest

open System
open Persimmon
open UseTestNameByReflection
open FSharp.Object.Diff

let ``push: does nothing with null object`` = test {
  let detector = CircularReferenceDetector(DetectorReferenceMatchingMode.EqualityOperator)
  detector.Push(null, null)
  do! assertEquals 0 detector.Size
}

let ``push: adds unknown object to stack`` = test {
  let detector = CircularReferenceDetector(DetectorReferenceMatchingMode.EqualityOperator)
  detector.Push("foo", NodePath.WithRoot())
  do! assertEquals 1 detector.Size
}

let ``push: throws CircularReferenceException when given known object`` = test {
  let detector = CircularReferenceDetector(DetectorReferenceMatchingMode.EqualityOperator)
  let rootPath = NodePath.WithRoot()
  detector.Push("foo", rootPath)
  let! ex = trap { it (detector.Push("foo", NodePath.With("test"))) }
  do! assertEquals typeof<CircularReferenceException> (ex.GetType())
  let (CircularReferenceException node) = ex
  do! assertEquals rootPath node
  do! assertEquals 1 detector.Size
}

let ``remove: does nothing with null object`` = test {
  let detector = CircularReferenceDetector(DetectorReferenceMatchingMode.EqualityOperator)
  do! assertEquals 0 detector.Size
  detector.Remove(null)
  do! assertEquals 0 detector.Size
}

let ``remove: throws IllegalArgumentException when instance is removed out of order`` = test {
  let detector = CircularReferenceDetector(DetectorReferenceMatchingMode.EqualityOperator)
  detector.Push("foo", null)
  let! ex = trap { it (detector.Remove("bar")) }
  do! assertEquals typeof<ArgumentException> (ex.GetType())
}

let ``remove: removes instance when it is removed in order`` = test {
  let detector = CircularReferenceDetector(DetectorReferenceMatchingMode.EqualityOperator)
  detector.Push("foo", null)
  do! assertEquals 1 detector.Size
  detector.Remove("foo")
  do! assertEquals 0 detector.Size
}

let ``knows: returns true when instance is known`` = test {
  let detector = CircularReferenceDetector(DetectorReferenceMatchingMode.EqualityOperator)
  do! assertPred (not (detector.Knows("foo")))
  detector.Push("foo", null)
  do! assertPred (detector.Knows("foo"))
}

let ``knows: returns false for previously added instance which has been removed`` = test {
  let detector = CircularReferenceDetector(DetectorReferenceMatchingMode.EqualityOperator)
  detector.Push("foo", null)
  do! assertPred (detector.Knows("foo"))
  detector.Remove("foo")
  do! assertPred (not (detector.Knows("foo")))
}

let ``knows: returns false when instance is unknown`` = test {
  let detector = CircularReferenceDetector(DetectorReferenceMatchingMode.EqualityOperator)
  detector.Push("foo", null)
  do! assertPred (not (detector.Knows("bar")))
}

let matchingMode = parameterize {
  source [
    (DetectorReferenceMatchingMode.EqualityOperator, box { Value = "foo" }, box { Value = "foo" }, false)
    (DetectorReferenceMatchingMode.ReferenceEqualMethod, box "foo", box "foo", true)
  ]
  run (fun (matchingMode, internalInstance, externalInstance, equalByOperator) -> test {
    let detector = CircularReferenceDetector(matchingMode)
    do! assertEquals equalByOperator (Object.ReferenceEquals(internalInstance, externalInstance))
    detector.Push(internalInstance, NodePath.WithRoot())
    do! assertPred (detector.Knows(externalInstance))
    let! ex = trap { it (detector.Push(internalInstance, NodePath.WithRoot())) }
    do! assertEquals typeof<CircularReferenceException> (ex.GetType())
    detector.Remove(externalInstance)
    do! assertPred (not (detector.Knows(externalInstance)))
    do! assertEquals 0 detector.Size
  })
}
