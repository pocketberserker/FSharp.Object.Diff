module CustomDiffer

open System.Text
open FSharp.Object.Diff

type ByteArrayDiffer = ByteArrayDiffer
with
  interface Differ with
    member __.Accepts(typ) = typ = typeof<byte []>
    member __.Compare(parentNode, instances) =
      let node = DiffNode(parentNode, instances.SourceAccessor)
      if instances.HasBeenAdded then node.State <- Added
      elif instances.HasBeenRemoved then node.State <- Removed
      else
        let baseValue = instances.TryGetBase<byte []>()
        let workingValue = instances.TryGetWorking<byte []>()
        if not <| (baseValue = workingValue) then
          node.State <- Changed
      node

type Factory = Factory
with
  interface DifferFactory with
    member __.CreateDiffer(_, _) = ByteArrayDiffer :> Differ

type User = {
  Id: int
  Name: string
  Avatar: byte []
}

let run () =
  let originalUser = { Id = 1; Name = "Foo"; Avatar = Encoding.UTF8.GetBytes("Test") }
  let updatedUser = { Id = 1; Name = "Foo"; Avatar = Encoding.UTF8.GetBytes("New Avatar") }
  let objectDiffer =
    ObjectDifferBuilder.StartBuilding()
      .Differs.Register(Factory)
      .Build()
  let node = objectDiffer.Compare(updatedUser, originalUser)
  node.Visit(PrintingVisitor(updatedUser, originalUser))
