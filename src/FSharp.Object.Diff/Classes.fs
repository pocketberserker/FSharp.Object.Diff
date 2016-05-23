[<AutoOpen>]
module internal FSharp.Object.Diff.TypeSyntax

open System
open System.Numerics
open System.Text
open System.Globalization
open System.Reflection

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
        typeof<'T>.GetConstructor(BindingFlags.Public ||| BindingFlags.NonPublic, null, [||], null)
      with _ -> null
    if ctor <> null then ctor.Invoke([||])
    else null

  member this.AllAssignableFrom(types: Type seq) =
    types |> Seq.forall (fun t -> this.IsAssignableFrom(t))

module Type =

  let isPrimitive (typ: Type) = typ <> null && typ.IsPrimitive

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

  let isComparable typ = typeof<IComparable>.IsAssignableFrom(typ)

  let private superclassesOf (types: Type seq) =
    let rec inner acc s = function
    | [] -> Seq.ofList acc
    | (x: Type)::xs ->
      let s = match s with | None -> x.BaseType | Some s -> s
      if s <> null && s <> typeof<obj> then
        inner (s::acc) (Some s.BaseType) (x::xs)
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
        if types |> Seq.filter (fun t -> potentiallySharedType.IsAssignableFrom(t)) |> Seq.length = Seq.length types then
          potentiallySharedType :: acc
        else acc
      ) []
      |> Seq.distinct
      |> Seq.toList
      |> List.sortWith (fun o1 o2 ->
        if o1.IsAssignableFrom(o2) then 1
        elif o2.IsAssignableFrom(o1) then -1
        else 0
      )
    if Seq.isEmpty sharedTypes then None
    else sharedTypes |> Seq.head |> Some
