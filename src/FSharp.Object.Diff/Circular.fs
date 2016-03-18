namespace FSharp.Object.Diff

open System

type CircularReferenceMatchingMode =
  | EqualityOperator
  | EqualMethod

type DetectorReferenceMatchingMode =
  | EqualityOperator
  | EqualMethod

exception CircularReferenceException of NodePath

type private CircularReferenceDetectorEntry = Entry of NodePath * obj
with
  override this.ToString() =
    match this with
    | Entry(nodePath, instance) ->
      sprintf "%O{%A}" nodePath instance

type CircularReferenceDetector(referenceMatchingMode: DetectorReferenceMatchingMode) =

  let stack = ResizeArray<CircularReferenceDetectorEntry>()

  let isMatch anObject anotherObject =
    match referenceMatchingMode with
    | EqualityOperator -> anotherObject = anObject
    | EqualMethod -> (anotherObject = anObject) || anObject.Equals(anotherObject)

  let entryForInstance instance =
    stack
    |> Seq.fold (fun result (Entry(_, i) as entry) ->
      match result with
      | Some _ -> result
      | None ->
        if isMatch instance i then Some entry
        else None
    ) None

  member __.Knows(needle: obj) =
    stack
    |> Seq.fold (fun result (Entry(_, instance)) ->
      if result then result
      else isMatch needle instance
    ) false

  member this.Push(instance: obj, nodePath: NodePath) =
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
      if isMatch instance (match Seq.last stack with | Entry(_, instance) -> instance) then
        stack.RemoveAt(stack.Count - 1)
      else
        raise <| ArgumentException("Detected inconsistency in enter/leave sequence. Must always be LIFO.")

  member __.Size = stack.Count

type CircularReferenceDetectorFactory =
  abstract member CreateCircularReferenceDetector: unit -> CircularReferenceDetector

type CircularReferenceExceptionHandler =
  abstract member OnCircularReferenceException: DiffNode -> unit
