module CrudBootstrap (crudBootstrap) where

import Prelude (Unit, identity, (#), ($), (>>>))

import CrudViewModel (createPerson, deletePerson, entries, isSelected, loadPeopleCatalogue, peopleDeleted, personLine, pick, refreshPeople, updatePerson)
import Data.Profunctor.Row.RecordToVariant as RecordToVariant
import Data.Profunctor.Row.VariantToVariant as VariantToVariant
import Data.Variant (match)
import Effect (Effect)
import PUI (action, atCase, blank, foreach, looped, toCase, updated, with)
import PUI.Web.Bootstrap (body, button, listGroup, listGroupItem, textField)
import PUI.Web (cl, clicked, clWhen, text, (:=))
import PUI.Web.HTML (div)
import QualifiedDo.Semigroupoid as Semigroupoid

crudBootstrap :: Effect Unit
crudBootstrap =
  body $
    ( Semigroupoid.do
      blank # action @{ "Filter prefix (surname)" :: String, "Name" :: String, "Surname" :: String, people :: Array { "Name" :: String, "Surname" :: String }, selected :: [ picked :: { index :: Int }, none :: {} ] } loadPeopleCatalogue
      ( Semigroupoid.do
        textField @"Filter prefix (surname)" {}
        textField @"Name" {}
        textField @"Surname" {}
        ( listGroup >>> cl "overflow-auto" >>> "style" := "max-height: 200px;" $
          ( clicked @"picked" _.key ( ( listGroupItem $ text personLine ) # cl "list-group-item-action" ) # clWhen isSelected "active" ) # foreach @"key" @( key :: Int, "Name" :: String, "Surname" :: String, status :: [ selected :: {}, unselected :: {} ] ) entries ) # updated (match { picked: pick })
        ( Semigroupoid.do
          ( div $ RecordToVariant.do
            button @"Create" {}
            button @"Update" {}
            button @"Delete" {} ) # cl "d-flex" # cl "gap-2"
          VariantToVariant.do
            blank # action @((Array { "Name" :: String, "Surname" :: String })) createPerson # atCase @"Create" # toCase @"created" identity
            blank # action @((Array { "Name" :: String, "Surname" :: String })) updatePerson # atCase @"Update" # toCase @"updated" identity
            blank # action @((Array { "Name" :: String, "Surname" :: String })) deletePerson # atCase @"Delete" # toCase @"deleted" identity ) # updated (match { created: refreshPeople, updated: refreshPeople, deleted: peopleDeleted }) ) # looped
    ) # with {}
