namespace FSharp.Object.Diff

[<AllowNullLiteral>]
type IdentityStrategy =
  abstract member Equals: obj * obj -> bool

type EqualsIdentityStrategy = EqualsIdentityStrategy
with
  interface IdentityStrategy with
    member __.Equals(working, base_) =
      obj.Equals(working, base_)
