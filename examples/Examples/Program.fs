module Program

open FSharp.Object.Diff

//type Person = {
//  FirstName: string
//  LastName: string
//}

[<AllowNullLiteral>]
type Person(firstName: string, lastName: string) =
  member __.FirstName = firstName
  member __.LastName = lastName
  override __.ToString() =
    match firstName, lastName with
    | null, null -> ""
    | firstName, null ->firstName
    | null, lastName ->lastName
    | firstName, lastName -> firstName + " " + lastName

[<EntryPoint>]
let main _ = 
    //let bruceWayne = { FirstName = "Bruce"; LastName = "Wayne" }
    //let batman = { FirstName = "Batman"; LastName = null }
    let bruceWayne = Person("Bruce", "Wayne")
    let batman = Person("Batman", null)
    let rootNode = ObjectDifferBuilder.BuildDefault().Compare(batman, bruceWayne)
    rootNode.Visit(NodeHierarchyVisitor(10))
    rootNode.Visit(PrintingVisitor(batman, bruceWayne))
    0
