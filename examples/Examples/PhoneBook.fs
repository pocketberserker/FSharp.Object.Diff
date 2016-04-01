module PhoneBook

open System.Collections.Generic
open FSharp.Object.Diff

[<ObjectDiffEqualsOnly>]
type PhoneNumber = {
  CountryCode: string
  AreaCode: string
  LocalNumber: string
}
  
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

  assert node.HasChanges
  assert node.HasChildren
  assert (node.ChildCount = 1)

  let contactsNode = node.Child("contacts")
  assert contactsNode.HasChanges

  let pinkmanNode = contactsNode.Child(CollectionItemElementSelector(jessePinkman))
  assert pinkmanNode.HasChanges

  let middleNameNode = pinkmanNode.Child("middleName")
  assert middleNameNode.HasChanges
  assert (middleNameNode.CanonicalGet(phoneBook) = null)
  assert (middleNameNode.CanonicalGet(modifiedPhoneBook) = box "Bruce")

  let whiteNode = contactsNode.Child(CollectionItemElementSelector(walterWhite))
  assert whiteNode.HasChanges

  let whiteMiddleNameNode = whiteNode.Child("middleName")
  assert whiteMiddleNameNode.HasChanges
  assert (whiteMiddleNameNode.CanonicalGet(phoneBook) = null)
  assert (whiteMiddleNameNode.CanonicalGet(modifiedPhoneBook) = box "Hartwell")
