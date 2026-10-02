module CrudBootstrap (crudBootstrap) where

import Prelude (Unit, identity, (#), ($), (>>>))

import CrudViewModel (createPerson, deletePerson, entries, isSelected, loadPeopleCatalogue, peopleDeleted, personLine, pick, refreshPeople, updatePerson)
import Data.Profunctor.Row.RecordToVariant as RecordToVariant
import Data.Profunctor.Row.VariantToVariant as VariantToVariant
import Data.Variant (match)
import Effect (Effect)
import PUI (action, atCase, blank, foreach, looped, state, toCase, updated, with)
import PUI.Web.Bootstrap (body, button, listGroup, listGroupItem, textField)
import PUI.Web (cl, clicked, clWhen, text, (:=))
import PUI.Web.HTML (div)
import QualifiedDo.Semigroupoid as Semigroupoid

crudBootstrap :: Effect Unit
crudBootstrap =
  body $
    ( Semigroupoid.do
      blank # action loadPeopleCatalogue
      ( Semigroupoid.do
        state @"people" @(Array { "Name" :: String, "Surname" :: String })
        state @"selected" @[ picked :: { index :: Int }, none :: {} ]
        textField @"Filter prefix (surname)" {}
        textField @"Name" {}
        textField @"Surname" {}
        ( listGroup >>> cl "overflow-auto" >>> "style" := "max-height: 200px;" $
          ( clicked @"picked" _.key ( Semigroupoid.do
            state @"Name" @String
            state @"Surname" @String
            state @"status" @[ selected :: {}, unselected :: {} ]
            ( listGroupItem $ text personLine ) # cl "list-group-item-action" ) # clWhen isSelected "active" ) # foreach @"key" @Int entries ) # updated (match { picked: pick })
        ( Semigroupoid.do
          ( div $ RecordToVariant.do
            button @"Create" {}
            button @"Update" {}
            button @"Delete" {} ) # cl "d-flex" # cl "gap-2"
          VariantToVariant.do
            blank # action @(Array { "Name" :: String, "Surname" :: String }) createPerson # atCase @"Create" # toCase @"created" identity
            blank # action @(Array { "Name" :: String, "Surname" :: String }) updatePerson # atCase @"Update" # toCase @"updated" identity
            blank # action @(Array { "Name" :: String, "Surname" :: String }) deletePerson # atCase @"Delete" # toCase @"deleted" identity ) # updated (match { created: refreshPeople, updated: refreshPeople, deleted: peopleDeleted }) ) # looped
    ) # with {}
