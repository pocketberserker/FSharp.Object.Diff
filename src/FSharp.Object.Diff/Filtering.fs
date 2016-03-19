namespace FSharp.Object.Diff

type IsReturnableResolver =
  abstract member IsReturnable: DiffNode -> bool
