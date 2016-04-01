namespace FSharp.Object.Diff.Tests

open System

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
