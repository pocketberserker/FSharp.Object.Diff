namespace FSharp.Object.Diff

type ObjectDiffer(dispatcher: DifferDispatcher) =
  member __.Compare(working, base_) =
//    dispatcher.ResetInstanceMemory()
//    try
      dispatcher.Dispatch(DiffNode.Root, Instances.Of(working, base_), RootAccessor)
//    finally
//      dispatcher.ClearInstanceMemory()
