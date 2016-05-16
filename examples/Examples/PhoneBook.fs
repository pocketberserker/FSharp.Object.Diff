module PhoneBook

open System
open System.Collections.Generic
open FSharp.Object.Diff

[<ObjectDiffEqualsOnly>]
type PhoneNumber = {
  CountryCode: string
  AreaCode: string
  LocalNumber: string
}
with
  override this.ToString() = this.CountryCode + " (" + this.AreaCode + ") " + this.LocalNumber
  
type Contact(firstName: string, lastName: string) =

  let phoneNumbers = Dictionary<string, PhoneNumber>()

  [<ObjectDiffProperty(Categories = [| "name" |])>]
  member __.FirstName = firstName

  [<ObjectDiffProperty(Categories = [| "name" |])>]
  member __.LastName = lastName

  [<ObjectDiffProperty(Categories = [| "name" |])>]
  member val MiddleName: string = null with get, set

  member __.PhoneNumbers = phoneNumbers

  member __.PhoneNumber with set(kind, phoneNumber) = phoneNumbers.Add(kind, phoneNumber)

  static member From(contact: Contact) =
    let copy = Contact(contact.FirstName, contact.LastName, MiddleName = contact.MiddleName)
    for KeyValue(kind, phoneNumber) in contact.PhoneNumbers do
      copy.PhoneNumber <- (kind, phoneNumber)
    copy

  override this.Equals(o) =
    match o with
    | null -> false
    | :? Contact as o ->
      if Object.ReferenceEquals(this, o) then true
      elif (if firstName <> null then firstName <> o.FirstName else o.FirstName <> null) then false
      elif (if lastName <> null then lastName <> o.LastName else o.LastName <> null) then false
      else true
    | _ -> false

  override __.GetHashCode() =
    let result = if firstName <> null then hash firstName else 0
    31 * result + (if lastName <> null then hash lastName else 0)

  override __.ToString() = [firstName; lastName] |> String.concat " "

type PhoneBook(name: string) =

  let mutable name = name
  let mutable contacts = ResizeArray<Contact>()

  member __.Name with get() = name and set(v) = name <- v

  member __.Contacts with get() = contacts and set(v) = contacts <- v

  member __.TryGetContact(firstName, lastName) =
    contacts
    |> Seq.tryFind (fun x -> x.FirstName = firstName && x.LastName = lastName)

  member __.AddContact(contact) = contacts.Add(contact)

  static member From(phoneBook: PhoneBook) =
    let copy = PhoneBook(phoneBook.Name)
    for contact in phoneBook.Contacts do
      copy.AddContact(Contact.From(contact))
    copy

let run () =

  let phoneBook = PhoneBook("Breaking Bad")

  let walterWhite = Contact("Walter", "White")
  walterWhite.PhoneNumber <- ("Home", { CountryCode = "1"; AreaCode = "505"; LocalNumber = "316-7871" })
  walterWhite.PhoneNumber <- ("Work", { CountryCode = "1"; AreaCode = "505"; LocalNumber = "456-3788" })
  phoneBook.AddContact(walterWhite)

  let jessePinkman = Contact("Jesse", "Pinkman")
  jessePinkman.PhoneNumber <- ("Home", { CountryCode = "1"; AreaCode = "505"; LocalNumber = "234-4628" })
  phoneBook.AddContact(jessePinkman)

  let modifiedPhoneBook = PhoneBook.From(phoneBook)
  modifiedPhoneBook.TryGetContact("Jesse", "Pinkman") |> Option.iter (fun x -> x.MiddleName <- "Bruce")
  modifiedPhoneBook.TryGetContact("Walter", "White") |> Option.iter (fun x -> x.MiddleName <- "Hartwell")

  let node = ObjectDifferBuilder.BuildDefault().Compare(modifiedPhoneBook, phoneBook)
  node.Visit(PrintingVisitor(modifiedPhoneBook, phoneBook))
