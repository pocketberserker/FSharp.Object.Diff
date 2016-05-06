module SimpleNode

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

let run () =
  let bruceWayne = { FirstName = "Bruce"; LastName = "Wayne" }
  let batman = { FirstName = "Batman"; LastName = null }
  let rootNode = ObjectDifferBuilder.BuildDefault().Compare(batman, bruceWayne)
  rootNode.Visit(NodeHierarchyVisitor(10))
  rootNode.Visit({ new PrintingVisitor(batman, bruceWayne) with
    override __.Filter(_) = true
  })
