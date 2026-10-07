module CrudBootstrap (crudBootstrap) where

import Prelude (Unit, identity, (#), ($), (>>>))

import CrudViewModel (createPerson, deletePerson, entries, isSelected, loadPeopleCatalogue, peopleLoadedLine, personCreatedLine, personDeletedLine, personLine, personNotCreatedLine, personNotDeletedLine, personNotUpdatedLine, personPickedLine, personUpdatedLine, pick, updatePerson)
import Data.Profunctor.Row.RecordToVariant as RecordToVariant
import Data.Profunctor.Row.VariantToRecord as VariantToRecord
import Data.Profunctor.Row.VariantToVariant as VariantToVariant
import Effect (Effect)
import PUI (action, atCase, fold, foreach, joined, looped, subChoice)
import PUI.Web.Bootstrap (body, button, indeterminateLinearProgress, listGroup, listGroupItem, textField, toast)
import PUI.Web (cl, clicked, clWhen, text, (:=))
import QualifiedDo.Semigroupoid as Semigroupoid

crudBootstrap :: Effect Unit
crudBootstrap =
  body $
    Semigroupoid.do
      indeterminateLinearProgress # action loadPeopleCatalogue
      toast @"People loaded" peopleLoadedLine # fold identity
      ( Semigroupoid.do
        textField @"Filter prefix (surname)" {}
        textField @"Name" {}
        textField @"Surname" {}
        RecordToVariant.do
          ( listGroup >>> cl "overflow-auto" >>> "style" := "max-height: 200px;" $
            ( clicked @"Person picked" _.key ( ( listGroupItem $ text personLine ) # cl "list-group-item-action" ) # clWhen isSelected "active" ) # foreach @"key"
              @( key :: Int
               , "Name" :: String
               , "Surname" :: String
               , status :: [ selected :: {}, unselected :: {} ]
               ) entries ) # joined @"Person picked"
          button @"Create" {}
          button @"Update" {}
          button @"Delete" {}
        ( VariantToVariant.do
          indeterminateLinearProgress # action createPerson # atCase @"Create"
          indeterminateLinearProgress # action updatePerson # atCase @"Update"
          indeterminateLinearProgress # action deletePerson # atCase @"Delete" ) # subChoice
        VariantToRecord.do
          toast @"Person picked" personPickedLine # fold pick
          toast @"Person created" personCreatedLine # fold identity
          toast @"Person not created" personNotCreatedLine # fold identity
          toast @"Person updated" personUpdatedLine # fold identity
          toast @"Person not updated" personNotUpdatedLine # fold identity
          toast @"Person deleted" personDeletedLine # fold identity
          toast @"Person not deleted" personNotDeletedLine # fold identity
      ) # looped
        @( "Filter prefix (surname)" :: String
         , "Name" :: String
         , "Surname" :: String
         , people :: Array { "Name" :: String, "Surname" :: String }
         , selected :: [ none :: {}, picked :: { index :: Int } ]
         )
