module Program

[<EntryPoint>]
let main args =
  match args with
  | [| "MapEntryValueAccess" |] -> MapEntryValueAccess.run ()
  | [| "CustomDiffer" |] -> CustomDiffer.run ()
  | [| "EqualsOnlyValueProviderMethod" |] -> EqualsOnlyValueProviderMethod.run ()
  | _ -> SimpleNode.run ()
  0
