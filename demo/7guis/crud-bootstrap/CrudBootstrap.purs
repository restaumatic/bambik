module CrudBootstrap (crudBootstrap) where

import Prelude (Unit, identity, (#), ($), (>>>))

import CrudViewModel (createPerson, deletePerson, entries, isSelected, loadPeopleCatalogue, personCreatedLine, personDeletedLine, personLine, personPickedLine, personUpdatedLine, pick, updatePerson)
import Data.Profunctor.Row.RecordToVariant as RecordToVariant
import Data.Profunctor.Row.VariantToVariant as VariantToVariant
import Data.Profunctor.Row.VariantToRecord as VariantToRecord
import Effect (Effect)
import PUI (action, atCase, blank, fold, foreach, joined, looped, subChoice, toCase)
import PUI.Web.Bootstrap (body, button, listGroup, listGroupItem, textField, toast)
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
            ( clicked @"Person picked" _.key ( ( listGroupItem $ text personLine ) # cl "list-group-item-action" ) # clWhen isSelected "active" ) # foreach @"key" @( key :: Int, "Name" :: String, "Surname" :: String, status :: [ selected :: {}, unselected :: {} ] ) entries ) # joined @"Person picked"
          ( div $ RecordToVariant.do
            button @"Create" {}
            button @"Update" {}
            button @"Delete" {} ) # cl "d-flex" # cl "gap-2"
        ( VariantToVariant.do
          blank # action createPerson # atCase @"Create" # toCase @"Person created" identity
          blank # action updatePerson # atCase @"Update" # toCase @"Person updated" identity
          blank # action deletePerson # atCase @"Delete" # toCase @"Person deleted" identity ) # subChoice
        VariantToRecord.do
          toast @"Person picked" personPickedLine # fold pick
          toast @"Person created" personCreatedLine # fold identity
          toast @"Person updated" personUpdatedLine # fold identity
          toast @"Person deleted" personDeletedLine # fold identity
      ) # looped
    )
