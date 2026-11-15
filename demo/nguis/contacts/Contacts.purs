module Contacts (Contact, Directory, Entry, Event, entries, entryLine, filed, isPicked, pickedContact, summaryLine) where

import Prelude ((#), (+), (/=), (<>), (==), class Applicative, map, max, show)

import Data.Array (filter, foldl, length, snoc)
import Data.Profunctor.Row.VariantToRecord as VariantToRecord
import Data.String (joinWith)
import Data.Variant (match)
import PUI (PUI, capture, fold, looped, with)
import QualifiedDo.Semigroupoid as Semigroupoid

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

type Event =
  [ "Contact edited" :: Directory
  , "Contact picked" :: { event :: Int, model :: Directory }
  , "Delete contact" :: Directory
  , "New contact" :: Directory
  ]

filed :: forall m. Applicative m => PUI m Directory Event -> PUI m {} Directory
filed view =
  Semigroupoid.do
    view
    VariantToRecord.do
      fold @"Contact picked" pick
      capture @"Contact edited"
      fold @"New contact" addContact
      fold @"Delete contact" removePicked
  # looped # with addressBook

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
