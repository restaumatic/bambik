module CrudBootstrap (crudBootstrap) where

import Prelude (Unit, identity, (#), ($), (>>>))

import CrudViewModel (createPerson, deletePerson, entries, isSelected, loadPeopleCatalogue, personLine, pick, updatePerson)
import Data.Profunctor.Row.RecordToVariant as RecordToVariant
import Data.Profunctor.Row.VariantToVariant as VariantToVariant
import Data.Profunctor.Row.VariantToRecord as VariantToRecord
import Effect (Effect)
import PUI (action, atCase, blank, fold, foreach, joined, looped, subChoice, toCase)
import PUI.Web.Bootstrap (body, button, listGroup, listGroupItem, textField)
import PUI.Web (cl, clicked, clWhen, text, (:=))
import PUI.Web.HTML (div)
import QualifiedDo.Semigroupoid as Semigroupoid

crudBootstrap :: Effect Unit
crudBootstrap =
  body $
    ( Semigroupoid.do
      blank # action @{ "Filter prefix (surname)" :: String, "Name" :: String, "Surname" :: String, people :: Array { "Name" :: String, "Surname" :: String }, selected :: [ none :: {}, picked :: { index :: Int } ] } loadPeopleCatalogue
      ( Semigroupoid.do
        textField @"Filter prefix (surname)" {}
        textField @"Name" {}
        textField @"Surname" {}
        RecordToVariant.do
          ( listGroup >>> cl "overflow-auto" >>> "style" := "max-height: 200px;" $
            ( clicked @"picked" _.key ( ( listGroupItem $ text personLine ) # cl "list-group-item-action" ) # clWhen isSelected "active" ) # foreach @"key" @( key :: Int, "Name" :: String, "Surname" :: String, status :: [ selected :: {}, unselected :: {} ] ) entries ) # joined @"picked"
          ( div $ RecordToVariant.do
            button @"Create" {}
            button @"Update" {}
            button @"Delete" {} ) # cl "d-flex" # cl "gap-2"
        ( VariantToVariant.do
          blank # action createPerson # atCase @"Create" # toCase @"created" identity
          blank # action updatePerson # atCase @"Update" # toCase @"updated" identity
          blank # action deletePerson # atCase @"Delete" # toCase @"deleted" identity ) # subChoice
        VariantToRecord.do
          fold @"picked" pick
          fold @"created" identity
          fold @"updated" identity
          fold @"deleted" identity
      ) # looped
    )
