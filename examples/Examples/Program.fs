module Program

[<EntryPoint>]
let main args =
  match args with
  | [| "MapEntryValueAccess" |] -> MapEntryValueAccess.run ()
  | [| "CustomDiffer" |] -> CustomDiffer.run ()
  | [| "EqualsOnlyValueProviderMethod" |] -> EqualsOnlyValueProviderMethod.run ()
  | [| "PhoneBook" |] -> PhoneBook.run ()
  | _ -> SimpleNode.run ()
  0
