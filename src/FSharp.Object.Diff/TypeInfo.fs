namespace FSharp.Object.Diff

open System

[<AllowNullLiteral>]
type InstanceFactory =
  abstract member NewInstanceOfType: Type -> obj

type PublicNoArgsConstructorInstanceFactory() =
  interface InstanceFactory with
    member __.NewInstanceOfType(typ: Type) =
      try
        typ.GetConstructor([||]).Invoke([||])
      with e ->
        raise <| TypeInitializationException(
          sprintf "Failed to create instance of type '%s'. Reason: %s" typ.FullName "Attempt to access the public no-args constructor caused an exception",
          e
        )

[<AllowNullLiteral>]
type TypeInfo(typ: Type) =
  let accessors = ResizeArray()
  let mutable instanceFactory: InstanceFactory = null
  member __.AddPropertyAccessor(propertyAccessor: PropertyAwareAccessor) =
    accessors.Add(propertyAccessor)
  member __.Type = typ
  member __.Accessors = List.ofSeq accessors
  member __.InstanceFactory with private get() = instanceFactory and set(value) = instanceFactory <- value
  member this.NewInstance() =
    this.InstanceFactory.NewInstanceOfType(typ)
