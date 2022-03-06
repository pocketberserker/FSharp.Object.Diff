[<AutoOpen>]
module internal FSharp.Object.Diff.Reflect

open System
open System.Numerics
open System.Text
open System.Globalization
open System.Reflection
open System.Linq

[<RequireQualifiedAccess>]
module Type =

  let isPrimitive (typ: Type) =
    typ <> null && typ.GetTypeInfo().IsPrimitive

  let private simpleTypes = [
    typeof<Type>
    typeof<Uri>
    typeof<CultureInfo>
    typeof<RegionInfo>
    // typeof<DateTimeFormatInfo> ???
    // typeof<NumberFormatInfo> ???
    typeof<Guid>

    typeof<BigInteger>
    typeof<string>
    typeof<StringBuilder>
    typeof<DateTime>
    typeof<DateTimeOffset>
    typeof<TimeSpan>
  ]

  let isSimple (typ: Type) =
    if typ = null then false
    elif isPrimitive typ then true
    elif typ = typeof<unit> then true
    else simpleTypes |> List.exists ((=) typ)

  let isAssignableFrom (a: Type) (b: Type) =
    a.GetTypeInfo().IsAssignableFrom(b.GetTypeInfo())

  let isComparable typ = isAssignableFrom typeof<IComparable> typ

  let private baseType (typ: Type) =
    typ.GetTypeInfo().BaseType

  let private superclassesOf (types: Type seq) =
    let rec inner acc s = function
    | [] -> Seq.ofList acc
    | (x: Type)::xs ->
      let s = match s with | None -> baseType x | Some s -> s
      if s <> null && s <> typeof<obj> then
        inner (s::acc) (Some(baseType s)) (x::xs)
      else inner acc None xs
    types
    |> Seq.toList
    |> inner [] None
    |> Seq.distinct

  let mostSpecificSharedType (types: Type seq) =
    seq { yield! superclassesOf(types); yield! types }
    |> Seq.distinct
    |> Seq.fold (fun acc potentiallySharedType ->
      if types |> Seq.filter (isAssignableFrom potentiallySharedType) |> Seq.length = Seq.length types then
        potentiallySharedType :: acc
      else acc
    ) []
    |> Seq.distinct
    |> Seq.toList
    |> List.sortWith (fun o1 o2 ->
      if isAssignableFrom o1 o2 then 1
      elif isAssignableFrom o2 o1 then -1
      else 0
    )
    |> Seq.tryHead

  let getMethod name (typ: Type) =
    typ.GetTypeInfo().GetDeclaredMethod(name)

  let getProperty name (typ: Type) =
    typ.GetTypeInfo().GetDeclaredProperty(name)

  let isGenericType (typ: Type) =
    typ.GetTypeInfo().IsGenericType

  let getGenericArguments (typ: Type) =
    typ.GetTypeInfo().GetGenericArguments()

  let getProperties (typ: Type) =
    typ.GetTypeInfo().DeclaredProperties
    |> Seq.toArray

  let getNonArgConstructor (t: Type) =
    t
      .GetTypeInfo().DeclaredConstructors
      .Where(fun x -> Array.isEmpty <| x.GetParameters())
      .FirstOrDefault()

  let isEnum (t: Type) =
    t.GetTypeInfo().IsEnum

  let getCustomAttributes<'T when 'T :> Attribute> inherit' (t: Type) =
    t.GetTypeInfo().GetCustomAttributes<'T>(inherit')
    |> Seq.toArray

[<RequireQualifiedAccess>]
module PropertyInfo =

  let getGetMethod (info: PropertyInfo) =
    info.GetMethod

  let getSetMethod (info: PropertyInfo) =
    info.SetMethod

  let getCustomAttributes (info: PropertyInfo) =
    info.GetCustomAttributes()

  let getCustomAttribute<'T when 'T :> Attribute> (info: PropertyInfo) =
    info.GetCustomAttribute<'T>()

type Type with

  static member TypesOf([<ParamArray>] values: obj[]) =
    values
    |> Array.choose (fun o ->
      if o <> null then Some (o.GetType())
      else None)
    |> Array.toSeq
    |> Seq.distinct

  member this.FreshInstanceOf() =
    let ctor =
      try
        Type.getNonArgConstructor this
      with _ -> null
    if ctor <> null then
      try
        ctor.Invoke([||])
      with _ -> null
    else null

  member this.AllAssignableFrom(types: Type seq) =
    types |> Seq.forall (fun t ->
      this.GetTypeInfo().IsAssignableFrom(t.GetTypeInfo())
    )
