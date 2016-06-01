namespace FSharp.Object.Diff.Tests

open System
open FSharp.Object.Diff

[<CustomEquality; CustomComparison>]
type ObjectWithIdentityAndValue = {
  Id: string
  Value: string
}
with
  override this.Equals(o) =
    if Object.ReferenceEquals(this, o) then true
    else
      match o with
      | :? ObjectWithIdentityAndValue as o ->
        this.Id = o.Id
      | _ -> false
  override this.GetHashCode() = hash this.Id
  override this.ToString() = sprintf "ObjectWithIdentityAndValue{id='%s', value='%s'}" this.Id this.Value
  interface IComparable with
    member this.CompareTo(o: obj) =
      match o with
      | :? ObjectWithIdentityAndValue as o ->
        if this.Id < o.Id then -1
        elif this.Id = o.Id then 0
        else 1
      | _ ->
        let this = hash this
        let o = hash o
        if this < o then -1
        elif this = o then 0
        else 1

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module ObjectWithIdentityAndValue =

  let idOnly id = { Id = id; Value = null }

[<CustomEquality; NoComparison>]
type ObjectWithString = {
  Value: string
}
with
  override this.Equals(o) =
    if Object.ReferenceEquals(this, o) then true
    else
      match o with
      | :? ObjectWithString as o ->
        if this.Value <> null then this.Value = o.Value else o.Value = null
      | _ -> false
  override this.GetHashCode() = if this.Value <> null then hash this.Value else 0
  override this.ToString() = sprintf "ObjectWithString[%s]" this.Value

type ObjectWithCircularReference = {
  Id: string
  Reference: ObjectWithCircularReference option
}
with
  override this.ToString() =
    sprintf "ObjectWithCircularReference{Id='%s'}" this.Id

[<AttributeUsage(AttributeTargets.Property, AllowMultiple = false); AllowNullLiteral>]
type ObjectDiffTestAttribute() = inherit Attribute()

type ObjectWithHashCodeAndEquals = {
  Key: string
  mutable Value: string
  mutable Item: ObjectWithHashCodeAndEquals option
}
with
  override this.ToString() = this.Key + ":" + this.Value

type ObjectWithAnnotatedProperty = {
  [<ObjectDiffProperty; ObjectDiffTestAttribute>]
  mutable Value: string
}
