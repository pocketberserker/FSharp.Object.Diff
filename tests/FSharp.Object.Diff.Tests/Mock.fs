namespace FSharp.Object.Diff.Tests

open System
open FSharp.Object.Diff

[<CustomEquality; NoComparison>]
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
