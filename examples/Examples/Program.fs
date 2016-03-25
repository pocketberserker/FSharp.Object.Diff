module Program

open FSharp.Object.Diff

type Person = {
  FirstName: string
  LastName: string
}
with
  override this.ToString() =
    match this.FirstName, this.LastName with
    | null, null -> ""
    | firstName, null ->firstName
    | null, lastName ->lastName
    | firstName, lastName -> firstName + " " + lastName

[<EntryPoint>]
let main _ = 
    let bruceWayne = { FirstName = "Bruce"; LastName = "Wayne" }
    let batman = { FirstName = "Batman"; LastName = null }
    let rootNode = ObjectDifferBuilder.BuildDefault().Compare(batman, bruceWayne)
    rootNode.Visit(NodeHierarchyVisitor(10))
    rootNode.Visit(PrintingVisitor(batman, bruceWayne))
    0
