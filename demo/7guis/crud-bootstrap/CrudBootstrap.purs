module CrudBootstrap (crudBootstrap) where

import Prelude (Unit, identity, (#), ($), (>>>))

import CrudViewModel (createPerson, deletePerson, entries, isSelected, loadPeopleCatalogue, peopleLoadedLine, personCreatedLine, personDeletedLine, personLine, personNotCreatedLine, personNotDeletedLine, personNotUpdatedLine, personPickedLine, personUpdatedLine, pick, updatePerson)
import Data.Profunctor.Row.RecordToVariant as RecordToVariant
import Data.Profunctor.Row.VariantToVariant as VariantToVariant
import Data.Profunctor.Row.VariantToRecord as VariantToRecord
import Effect (Effect)
import PUI (action, atCase, blankStatus, fold, foreach, joined, looped, subChoice)
import PUI.Web.Bootstrap (body, button, listGroup, listGroupItem, textField, toast)
import PUI.Web (cl, clicked, clWhen, text, (:=))
import PUI.Web.HTML (div)
import QualifiedDo.Semigroupoid as Semigroupoid

crudBootstrap :: Effect Unit
crudBootstrap =
  body $
    ( Semigroupoid.do
      blankStatus @"People loaded" # action loadPeopleCatalogue
      toast @"People loaded" peopleLoadedLine # fold identity
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
          ( VariantToRecord.do
            blankStatus @"Person created"
            blankStatus @"Person not created" ) # action createPerson # atCase @"Create"
          ( VariantToRecord.do
            blankStatus @"Person updated"
            blankStatus @"Person not updated" ) # action updatePerson # atCase @"Update"
          ( VariantToRecord.do
            blankStatus @"Person deleted"
            blankStatus @"Person not deleted" ) # action deletePerson # atCase @"Delete" ) # subChoice
        VariantToRecord.do
          toast @"Person picked" personPickedLine # fold pick
          toast @"Person created" personCreatedLine # fold identity
          toast @"Person not created" personNotCreatedLine # fold identity
          toast @"Person updated" personUpdatedLine # fold identity
          toast @"Person not updated" personNotUpdatedLine # fold identity
          toast @"Person deleted" personDeletedLine # fold identity
          toast @"Person not deleted" personNotDeletedLine # fold identity
      ) # looped @( "Filter prefix (surname)" :: String, "Name" :: String, "Surname" :: String, people :: Array { "Name" :: String, "Surname" :: String }, selected :: [ none :: {}, picked :: { index :: Int } ] )
    )
