module CrudMDC2 (crudMDC2) where

import Prelude (Unit, identity, (#), ($))

import CrudViewModel (createPerson, deletePerson, entries, isSelected, loadPeopleCatalogue, peopleDeleted, personLine, pick, refreshPeople, updatePerson)
import Data.Profunctor.Row.RecordToVariant as RecordToVariant
import Data.Profunctor.Row.VariantToVariant as VariantToVariant
import Data.Variant (match)
import Effect (Effect)
import PUI (action, atCase, looped, toCase, updated, with)
import PUI.Web (shown, text)
import PUI.Web.MDC2 (body, button, cardActions, filledTextField, indeterminateLinearProgress, listOf)
import QualifiedDo.Semigroupoid as Semigroupoid

crudMDC2 :: Effect Unit
crudMDC2 =
  body $
    ( Semigroupoid.do
      indeterminateLinearProgress @"Loading people" # action @{ "Filter prefix (surname)" :: String, "Name" :: String, "Surname" :: String, people :: Array { "Name" :: String, "Surname" :: String }, selected :: [ picked :: { index :: Int }, none :: {} ] } loadPeopleCatalogue
      ( Semigroupoid.do
        filledTextField @"Filter prefix (surname)" {}
        filledTextField @"Name" {}
        filledTextField @"Surname" {}
        listOf @"picked" @"key" @( key :: Int, "Name" :: String, "Surname" :: String, status :: [ selected :: {}, unselected :: {} ] ) { selected: isSelected } entries (text personLine # shown) # updated (match { picked: pick })
        ( Semigroupoid.do
          cardActions $ RecordToVariant.do
            button @"Create" {}
            button @"Update" {}
            button @"Delete" {}
          VariantToVariant.do
            indeterminateLinearProgress @"Creating person" # action @((Array { "Name" :: String, "Surname" :: String })) createPerson # atCase @"Create" # toCase @"created" identity
            indeterminateLinearProgress @"Updating person" # action @((Array { "Name" :: String, "Surname" :: String })) updatePerson # atCase @"Update" # toCase @"updated" identity
            indeterminateLinearProgress @"Deleting person" # action @((Array { "Name" :: String, "Surname" :: String })) deletePerson # atCase @"Delete" # toCase @"deleted" identity ) # updated (match { created: refreshPeople, updated: refreshPeople, deleted: peopleDeleted }) ) # looped
    ) # with {}
