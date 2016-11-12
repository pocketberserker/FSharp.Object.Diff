module FSharp.Object.Diff.Tests.DifferProviderTest

open System
open Persimmon
open UseTestNameByReflection
open FSharp.Object.Diff
open Foq


let ``return differ that accepts given type`` = test {
  let differProvider = DifferProvider()
  let differMock =
    Mock<Differ>.With(fun x -> <@ x.Accepts(typeof<string>) --> true @>)
  differProvider.Push(differMock)
  do! assertPred (Object.ReferenceEquals(differProvider.RetrieveDifferForType(typeof<string>), differMock))
}

let ``return the last pushed differ that accepts the given type`` = test {
  let differProvider = DifferProvider()
  let differ1 =
    Mock<Differ>.With(fun x -> <@ x.Accepts(typeof<string>) --> true @>)
  let differ2 =
    Mock<Differ>.With(fun x -> <@ x.Accepts(typeof<string>) --> true @>)
  differProvider.Push(differ2)
  do! assertPred (not <| Object.ReferenceEquals(differProvider.RetrieveDifferForType(typeof<string>), differ1))
  do! assertPred (Object.ReferenceEquals(differProvider.RetrieveDifferForType(typeof<string>), differ2))
}

let ``throw IllegalArgumentException if no type is given`` = test {
  let differProvider = DifferProvider()
  let! ex = trap { it (differProvider.RetrieveDifferForType(null)) }
  do! assertEquals typeof<ArgumentException> (ex.GetType())
}

let ``throw InvalidOperationException if no differ exists for given type`` = test {
  let differProvider = DifferProvider()
  differProvider.Push(Mock<Differ>.With(fun x -> <@ x.Accepts(typeof<string>) --> true @>))
  let! ex = trap { it (differProvider.RetrieveDifferForType(typeof<DateTime>)) }
  do! assertEquals typeof<InvalidOperationException> (ex.GetType())
}
