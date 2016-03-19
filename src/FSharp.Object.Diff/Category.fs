namespace FSharp.Object.Diff

open System

type CategoryResolver =
  abstract member ResolveCategories: DiffNode -> Set<string>
