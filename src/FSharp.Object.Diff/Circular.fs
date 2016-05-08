namespace FSharp.Object.Diff

open System

type CircularReferenceMatchingMode =
  | EqualityOperator
  | ReferenceEqualMethod

type DetectorReferenceMatchingMode =
  | EqualityOperator
  | ReferenceEqualMethod

exception CircularReferenceException of NodePath

type private CircularReferenceDetectorEntry = Entry of NodePath * obj
with
  override this.ToString() =
    match this with
    | Entry(nodePath, instance) ->
      sprintf "%O{%A}" nodePath instance

[<AllowNullLiteral>]
type CircularReferenceDetector(referenceMatchingMode: DetectorReferenceMatchingMode) =

  let stack = ResizeArray<CircularReferenceDetectorEntry>()

  let isMatch anObject anotherObject =
    match referenceMatchingMode with
    | EqualityOperator -> anotherObject = anObject
    | ReferenceEqualMethod -> Object.ReferenceEquals(anotherObject, anObject)

  let entryForInstance instance =
    stack
    |> Seq.tryFind (fun (Entry(_, i)) -> isMatch instance i)

  member __.Knows(needle: obj) =
    stack
    |> Seq.exists (fun (Entry(_, instance)) -> isMatch needle instance)

  abstract member Push: obj * NodePath -> unit
  default this.Push(instance, nodePath) =
    if instance = null then ()
    else
      if this.Knows(instance) then
        let path =
          match entryForInstance instance with
          | Some(Entry(path, _)) -> path
          | None -> null
        raise <| CircularReferenceException(path)
      stack.Add(Entry(nodePath, instance))

  member __.Remove(instance: obj) =
    if instance = null then ()
    else
      if not <| Seq.isEmpty stack then
        if isMatch instance (match Seq.last stack with | Entry(_, instance) -> instance) then
          stack.RemoveAt(stack.Count - 1)
        else
          raise <| ArgumentException("Detected inconsistency in enter/leave sequence. Must always be LIFO.")

  member __.Size = stack.Count

type CircularReferenceDetectorFactory =
  abstract member CreateCircularReferenceDetector: unit -> CircularReferenceDetector

type CircularReferenceExceptionHandler =
  abstract member OnCircularReferenceException: DiffNode -> unit
