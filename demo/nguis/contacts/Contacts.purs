module Contacts (Contact, Directory, Entry, addContact, addressBook, entries, entryLine, isPicked, pick, pickedContact, removePicked, summaryLine) where

import Prelude ((+), (/=), (<>), (==), map, max, show)

import Data.Array (filter, foldl, length, snoc)
import Data.String (joinWith)
import Data.Variant (match)

type Contact =
  { "Kind" :: [ "Company" :: {}, "Person" :: {} ]
  , "Name" :: String
  , "Phone" :: String
  , "Tax number" :: String
  , id :: Int
  }

type Directory =
  { contacts :: Array Contact
  , selected :: [ none :: {}, picked :: { id :: Int } ]
  }

type Entry = { id :: Int, name :: String, phone :: String, status :: [ selected :: {}, unselected :: {} ] }

addressBook :: Directory
addressBook =
  { contacts:
    [ { id: 1, "Kind": ."Person" {}, "Name": "Ada Lovelace", "Phone": "+44 20 7946 0958", "Tax number": "" }
    , { id: 2, "Kind": ."Company" {}, "Name": "Analytical Engines Ltd", "Phone": "+44 20 7946 0101", "Tax number": "GB 123 4567 89" }
    , { id: 3, "Kind": ."Person" {}, "Name": "Grace Hopper", "Phone": "+1 212 555 0199", "Tax number": "" }
    ]
  , selected: .none {}
  }

summaryLine :: Directory -> String
summaryLine { contacts } = counted (length contacts) "contact" <> ", " <> counted (length (filter isCompany contacts)) "company"
  where
  isCompany c = c."Kind" == ."Company" {}
  counted 1 noun = "1 " <> noun
  counted n "company" = show n <> " companies"
  counted n noun = show n <> " " <> noun <> "s"

entries :: Directory -> Array Entry
entries { contacts, selected } = map entry contacts
  where
  entry { id, "Name": name, "Phone": phone } = { id, name, phone, status: if selected == .picked { id } then .selected {} else .unselected {} }

entryLine :: Entry -> String
entryLine { name, phone } = joinWith " · " (filter (_ /= "") [ if name == "" then "Unnamed" else name, phone ])

isPicked :: Entry -> Boolean
isPicked { status } = match { selected: \_ -> true, unselected: \_ -> false } status

pickedContact :: Directory -> Array Contact
pickedContact { contacts, selected } = match
  { picked: \{ id } -> filter (\c -> c.id == id) contacts
  , none: \_ -> []
  } selected

pick :: { event :: Int, model :: Directory } -> Directory
pick { event: id, model: directory } = directory { selected = .picked { id } }

addContact :: Directory -> Directory
addContact directory@{ contacts } = directory
  { contacts = snoc contacts { id, "Kind": ."Person" {}, "Name": "", "Phone": "", "Tax number": "" }
  , selected = .picked { id }
  }
  where
  id = foldl max 0 (map _.id contacts) + 1

removePicked :: Directory -> Directory
removePicked directory@{ contacts, selected } = match
  { picked: \{ id } -> directory { contacts = filter (\c -> c.id /= id) contacts, selected = .none {} }
  , none: \_ -> directory
  } selected
