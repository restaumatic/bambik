module Crud.Bootstrap (crud) where

import Prelude (Unit, (#), ($), (>>>))

import Crud (createPerson, deletePerson, entries, isSelected, loadPeopleCatalogue, personCreatedLine, personDeletedLine, personLine, personNotCreatedLine, personNotDeletedLine, personNotUpdatedLine, personUpdatedLine, pick, updatePerson)
import Data.Profunctor.Row.RecordToVariant as RecordToVariant
import Data.Profunctor.Row.VariantToRecord as VariantToRecord
import Effect (Effect)
import PUI (action, anyOf, atCase, captured, fold, joined, looped, unshown)
import PUI.Web.Bootstrap (body, button, indeterminateLinearProgress, listGroup, listGroupItem, textField, toast)
import PUI.Web (cl, clicked, clWhen, text, (:=))
import QualifiedDo.Semigroupoid as Semigroupoid

crud :: Effect Unit
crud =
  body $ Semigroupoid.do
    indeterminateLinearProgress # action loadPeopleCatalogue
    unshown @"People loaded" # captured
    ( Semigroupoid.do
      textField @"Filter prefix (surname)"
      textField @"Name"
      textField @"Surname"
      RecordToVariant.do
        ( listGroup >>> cl "overflow-auto" >>> "style" := "max-height: 200px;" $
          ( clicked @"Person picked" @"key" ( ( listGroupItem $ text personLine ) # cl "list-group-item-action" ) # clWhen isSelected "active" ) # anyOf @"key"
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
            toast @"Person created" personCreatedLine # captured
            toast @"Person not created" personNotCreatedLine # captured ) # atCase @"Create"
        ( Semigroupoid.do
          indeterminateLinearProgress # action updatePerson
          VariantToRecord.do
            toast @"Person updated" personUpdatedLine # captured
            toast @"Person not updated" personNotUpdatedLine # captured ) # atCase @"Update"
        ( Semigroupoid.do
          indeterminateLinearProgress # action deletePerson
          VariantToRecord.do
            toast @"Person deleted" personDeletedLine # captured
            toast @"Person not deleted" personNotDeletedLine # captured ) # atCase @"Delete"
    ) # looped
      @( "Filter prefix (surname)" :: String
       , "Name" :: String
       , "Surname" :: String
       , people :: Array { "Name" :: String, "Surname" :: String }
       , selected :: [ none :: {}, picked :: { index :: Int } ]
       )
