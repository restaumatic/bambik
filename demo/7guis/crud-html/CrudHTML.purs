module CrudHTML (crudHTML) where

import Prelude (Unit, identity, (#), ($), (<>), (>>>))

import CrudViewModel (createPerson, deletePerson, entries, loadPeopleCatalogue, peopleDeleted, personLine, pick, refreshPeople, updatePerson)
import Data.Profunctor.Row.RecordToVariant as RecordToVariant
import Data.Profunctor.Row.VariantToVariant as VariantToVariant
import Data.Variant (match)
import Effect (Effect)
import PUI (action, atCase, blank, foreach, looped, toCase, updated, with)
import PUI.Web (attrWith, clicked, shown, staticText, text, (:=))
import PUI.Web.HTML (body, button, div, input, label, li, p, ul)
import QualifiedDo.Semigroupoid as Semigroupoid

crudHTML :: Effect Unit
crudHTML =
  body $ div $ ( Semigroupoid.do
    blank # action @{ "Filter prefix (surname)" :: String, "Name" :: String, "Surname" :: String, people :: Array { "Name" :: String, "Surname" :: String }, selected :: [ picked :: { index :: Int }, none :: {} ] } loadPeopleCatalogue
    ( Semigroupoid.do
      p ( label $ Semigroupoid.do
        (staticText @"Filter prefix (surname) ") # shown
        input @"Filter prefix (surname)" "text" )
      p ( label $ Semigroupoid.do
        (staticText @"Name ") # shown
        input @"Name" "text" )
      p ( label $ Semigroupoid.do
        (staticText @"Surname ") # shown
        input @"Surname" "text" )
      ( ul >>> "style" := "list-style: none; margin: 0; padding: 0; border: 1px solid #ccc; max-height: 200px; overflow: auto; width: 100%;" $
        ( clicked @"picked" _.key ( li >>> attrWith "style" entryFace $ text personLine # shown ) ) # foreach @"key" @( key :: Int, "Name" :: String, "Surname" :: String, status :: [ selected :: {}, unselected :: {} ] ) entries ) # updated (match { picked: pick })
      ( Semigroupoid.do
        div $ RecordToVariant.do
          button @"Create" {}
          button @"Update" {}
          button @"Delete" {}
        VariantToVariant.do
          blank # action @((Array { "Name" :: String, "Surname" :: String })) createPerson # atCase @"Create" # toCase @"created" identity
          blank # action @((Array { "Name" :: String, "Surname" :: String })) updatePerson # atCase @"Update" # toCase @"updated" identity
          blank # action @((Array { "Name" :: String, "Surname" :: String })) deletePerson # atCase @"Delete" # toCase @"deleted" identity ) # updated (match { created: refreshPeople, updated: refreshPeople, deleted: peopleDeleted }) ) # looped
  ) # with {}

entryFace :: { key :: Int, "Name" :: String, "Surname" :: String, status :: [ selected :: {}, unselected :: {} ] } -> String
entryFace { status } = "padding: 4px 8px; cursor: pointer;" <> match { selected: \_ -> " background: #cde;", unselected: \_ -> "" } status
