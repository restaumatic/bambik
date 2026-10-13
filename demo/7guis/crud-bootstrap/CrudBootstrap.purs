module CrudBootstrap (crudBootstrap) where

import Prelude (Unit, identity, (#), ($), (>>>))

import CrudViewModel (createPerson, deletePerson, entries, isSelected, loadPeopleCatalogue, personCreatedLine, personDeletedLine, personLine, personNotCreatedLine, personNotDeletedLine, personNotUpdatedLine, personUpdatedLine, pick, updatePerson)
import Data.Profunctor.Row.RecordToVariant as RecordToVariant
import Data.Profunctor.Row.VariantToRecord as VariantToRecord
import Effect (Effect)
import PUI (action, atCase, fold, foreach, joined, looped, unshown)
import PUI.Web.Bootstrap (body, button, indeterminateLinearProgress, listGroup, listGroupItem, textField, toast)
import PUI.Web (cl, clicked, clWhen, text, (:=))
import QualifiedDo.Semigroupoid as Semigroupoid

crudBootstrap :: Effect Unit
crudBootstrap =
  body $ Semigroupoid.do
    indeterminateLinearProgress # action loadPeopleCatalogue
    unshown @"People loaded" # fold identity
    ( Semigroupoid.do
      textField @"Filter prefix (surname)"
      textField @"Name"
      textField @"Surname"
      RecordToVariant.do
        ( listGroup >>> cl "overflow-auto" >>> "style" := "max-height: 200px;" $
          ( clicked @"Person picked" @"key" ( ( listGroupItem $ text personLine ) # cl "list-group-item-action" ) # clWhen isSelected "active" ) # foreach @"key"
            @( key :: Int
             , "Name" :: String
             , "Surname" :: String
             , status :: [ selected :: {}, unselected :: {} ]
             ) entries ) # joined @"Person picked"
        button @"Create"
        button @"Update"
        button @"Delete"
      VariantToRecord.do
        unshown @"Person picked" # fold pick
        ( Semigroupoid.do
          indeterminateLinearProgress # action createPerson
          VariantToRecord.do
            toast @"Person created" personCreatedLine # fold identity
            toast @"Person not created" personNotCreatedLine # fold identity ) # atCase @"Create"
        ( Semigroupoid.do
          indeterminateLinearProgress # action updatePerson
          VariantToRecord.do
            toast @"Person updated" personUpdatedLine # fold identity
            toast @"Person not updated" personNotUpdatedLine # fold identity ) # atCase @"Update"
        ( Semigroupoid.do
          indeterminateLinearProgress # action deletePerson
          VariantToRecord.do
            toast @"Person deleted" personDeletedLine # fold identity
            toast @"Person not deleted" personNotDeletedLine # fold identity ) # atCase @"Delete"
    ) # looped
      @( "Filter prefix (surname)" :: String
       , "Name" :: String
       , "Surname" :: String
       , people :: Array { "Name" :: String, "Surname" :: String }
       , selected :: [ none :: {}, picked :: { index :: Int } ]
       )
