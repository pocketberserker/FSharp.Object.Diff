[<AutoOpen>]
module internal FSharp.Object.Diff.Reflect

open System
open System.Numerics
open System.Text
open System.Globalization
open System.Reflection
#if PCL
open System.Linq
#endif

[<RequireQualifiedAccess>]
module Type =

  let isPrimitive (typ: Type) =
    typ <> null &&
    typ
#if PCL
      .GetTypeInfo()
#endif
      .IsPrimitive

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
    a
#if PCL
      .GetTypeInfo().IsAssignableFrom(b.GetTypeInfo())
#else
      .IsAssignableFrom(b)
#endif

  let isComparable typ = isAssignableFrom typeof<IComparable> typ

  let private baseType (typ: Type) =
    typ
#if PCL
      .GetTypeInfo().BaseType
#else
      .BaseType
#endif

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

  let  mostSpecificSharedType (types: Type seq) =
    let sharedTypes =
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
    if Seq.isEmpty sharedTypes then None
    else sharedTypes |> Seq.head |> Some

  let getMethod name (typ: Type) =
    typ
#if PCL
      .GetTypeInfo().GetDeclaredMethod(name)
#else
      .GetMethod(name)
#endif

  let getProperty name (typ: Type) =
    typ
#if PCL
      .GetTypeInfo().GetDeclaredProperty(name)
#else
      .GetProperty(name)
#endif

  let isGenericType (typ: Type) =
    typ
#if PCL
      .GetTypeInfo().IsGenericType
#else
      .IsGenericType
#endif

  let getGenericArguments (typ: Type) =
    typ
#if PCL
      .GetTypeInfo().GetGenericParameterConstraints()
#else
      .GetGenericArguments()
#endif

  let getProperties (typ: Type) =
    typ
#if PCL
      .GetTypeInfo().DeclaredProperties
    |> Seq.toArray
#else
      .GetProperties()
#endif

  let getNonArgConstructor (t: Type) =
    t
#if PCL
      .GetTypeInfo().DeclaredConstructors
      .Where(fun x -> Array.isEmpty <| x.GetParameters())
      .FirstOrDefault()
#else
      .GetConstructor(BindingFlags.Public ||| BindingFlags.NonPublic, null, [||], null)
#endif

  let isEnum (t: Type) =
    t
#if PCL
      .GetTypeInfo().IsEnum
#else
      .IsEnum
#endif

  let getCustomAttributes<'T when 'T :> Attribute> inherit' (t: Type) =
    t
#if PCL
      .GetTypeInfo().GetCustomAttributes<'T>(inherit')
    |> Seq.toArray
#else
      .GetCustomAttributes(typeof<'T>, inherit')
    |> Array.map (fun x -> x :?> 'T)
#endif

[<RequireQualifiedAccess>]
module PropertyInfo =

  let getGetMethod (info: PropertyInfo) =
    info
#if PCL
      .GetMethod
#else
      .GetGetMethod()
#endif

  let getSetMethod (info: PropertyInfo) =
    info
#if PCL
      .SetMethod
#else
      .GetSetMethod()
#endif

  let getCustomAttributes (info: PropertyInfo) =
#if PCL
    info.GetCustomAttributes()
#else
    Attribute.GetCustomAttributes(info)
    |> Array.toSeq
#endif

  let getCustomAttribute<'T when 'T :> Attribute> (info: PropertyInfo) =
#if PCL
    info.GetCustomAttribute<'T>()
#else
    Attribute.GetCustomAttribute(info, typeof<'T>) :?> 'T
#endif

type Type with

  static member TypesOf([<ParamArray>] values: obj[]) =
    values
    |> Array.choose (fun o ->
      if o <> null then Some (o.GetType())
      else None)
    |> Array.toSeq
    |> Seq.distinct

  static member FreshInstanceOf<'T>() =
    let ctor =
      try
        Type.getNonArgConstructor typeof<'T>
      with _ -> null
    if ctor <> null then ctor.Invoke([||])
    else null

  member this.AllAssignableFrom(types: Type seq) =
    types |> Seq.forall (fun t ->
#if PCL
      this.GetTypeInfo().IsAssignableFrom(t.GetTypeInfo())
#else
      this.IsAssignableFrom(t)
#endif
    )
