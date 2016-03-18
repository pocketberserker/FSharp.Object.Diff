namespace FSharp.Object.Diff

open System

type CategoryResolver =
  abstract member ResolveCategories: DiffNode -> Set<string>

type CategoryConfigurerOf =
    abstract member ToBe: string[] -> CategoryConfigurer

and CategoryConfigurer =
  abstract member OfNode: NodePath -> CategoryConfigurerOf
  abstract member OfType: Type -> CategoryConfigurerOf

