namespace FSharp.Object.Diff

open System
open System.Reflection

type IsIntrospectableResolver =
  abstract member IsIntrospectable: DiffNode -> bool

type Introspector =
  abstract member Introspect: Type -> TypeInfo

type TypeInfoResolver =
  abstract member TypeInfoForNode: DiffNode -> TypeInfo

[<AbstractClass>]
type PropertyAccessException(propertyName: string, targetType: Type, cause: exn) =
  inherit exn("", cause)
  member __.PropertyName = propertyName
  member __.TargetType = targetType

type PropertyReadException(propertyName: string, targetType: Type, cause: exn) =
  inherit PropertyAccessException(propertyName, targetType, cause)
  override __.Message = sprintf "Failed to read value from property '%s' of type '%s'" propertyName targetType.FullName

type PropertyWriteException(propertyName: string, targetType: Type, newValue: obj, cause: exn) =
  inherit PropertyAccessException(propertyName, targetType, cause)
  override __.Message =
    sprintf "Failed to write new value '%A' to property '%s' of type '%s'" newValue propertyName targetType.FullName
  member __.NewValue = newValue

[<AllowNullLiteral>]
type PropertyAccessExceptionHandler =
  abstract member OnPropertyReadException: PropertyReadException * DiffNode -> unit

type PropertyAccessExceptionHandlerResolver =
  abstract member ResolvePropertyAccessExceptionHandler: Type * string -> PropertyAccessExceptionHandler

type InstanceFactoryFallbackDecorator(instanceFactory: InstanceFactory) =
  interface InstanceFactory with
    member __.NewInstanceOfType(typ: Type) =
      let instance = instanceFactory.NewInstanceOfType(typ)
      if instance <> null then instance
      // TODO: implement PublicNoArgsConstructorInstanceFactory
      else raise <| NotImplementedException()

type DefaultPropertyAccessExceptionHandler() =
  interface PropertyAccessExceptionHandler with
    member __.OnPropertyReadException(e, _) = raise e

type PropertyAccessor(propertyName: string, readMethod: MethodInfo, writeMethod: MethodInfo) =
  let rec getFieldAttributes (typ: Type) =
    try
      Attribute.GetCustomAttributes(typ.GetField(propertyName))
      |> Array.toSeq
      |> Seq.distinct
    with _ ->
      if typ.BaseType <> null then getFieldAttributes typ.BaseType
      else Seq.empty
  member __.GetReadMethodAttributes() =
    Attribute.GetCustomAttributes(readMethod)
    |> Array.toSeq
    |> Seq.distinct
  member __.GetFieldAttributes() = readMethod.DeclaringType |> getFieldAttributes
//  TODO: implement
//  interface PropertyAwareAccessor with
//    member __.PropertyName = propertyName
//    member this.GetFieldAttributes() = this.GetFieldAttributes()
//    member this.GetFieldAttribute<'T when 'T :> Attribute and 'T : null>() =
//      this.GetFieldAttributes()
//      |> Seq.tryFind (fun a -> typeof<'T>.IsAssignableFrom(a.GetType()))
//      |> function | Some v -> v :?> 'T | None -> null
//    member this.GetReadMethodAttributes() = this.GetReadMethodAttributes()
//    member this.GetReadMethodAttribute<'T when 'T :> Attribute and 'T : null>() =
//      this.GetReadMethodAttributes()
//      |> Seq.tryFind (fun a -> typeof<'T>.IsAssignableFrom(a.GetType()))
//      |> function | Some v -> v :?> 'T | None -> null
//    member __.ElementSelector = BeanPropertyElementSelector(propertyName) :> ElementSelector

type StandardIntrospector = StandardIntrospector
with
  interface Introspector with
    member __.Introspect(typ: Type) =
      let typeInfo = TypeInfo(typ)
//      TODO: implement
//      typ.GetProperties()
//      |> Array.iter (fun x ->
//        typeInfo.AddPropertyAccessor(PropertyAccessor(x.Name, x.GetGetMethod(), x.GetSetMethod()))
//      )
      typeInfo
