module Crud.Bootstrap (crud) where

import Prelude ((#), ($), (>>>), identity, Unit)

import Crud (createPerson, deletePerson, entries, isSelected, loadPeopleCatalogue, personLine, pick, updatePerson)
import Data.Profunctor.Row.RecordToVariant as RecordToVariant
import Data.Profunctor.Row.VariantToRecord as VariantToRecord
import Effect (Effect)
import PUI (action, anyOf, atCase, capture, fold, joined, looped)
import PUI.Web.Bootstrap (body, button, indeterminateLinearProgress, listGroup, listGroupItem, textField, toast)
import PUI.Web (cl, clicked, clWhen, text, (:=))
import QualifiedDo.Semigroupoid as Semigroupoid

crud :: Effect Unit
crud =
  body $ Semigroupoid.do
    indeterminateLinearProgress # action loadPeopleCatalogue
    capture @"People loaded"
    ( Semigroupoid.do
      textField @"Filter prefix (surname)"
      textField @"Name"
      textField @"Surname"
      RecordToVariant.do
        ( listGroup >>> cl "overflow-auto" >>> "style" := "max-height: 200px;" $
          ( clicked @"Person picked" @"key" $ listGroupItem >>> cl "list-group-item-action" >>> clWhen isSelected "active" $ text personLine ) # anyOf @"key"
            @( key :: Int
             , "Name" :: String
             , "Surname" :: String
             , status :: [ selected :: {}, unselected :: {} ]
             ) entries ) # joined @"Person picked"
        button @"Create"
        button @"Update"
        button @"Delete"
      VariantToRecord.do
        fold @"Person picked" pick
        ( Semigroupoid.do
          indeterminateLinearProgress # action createPerson
          VariantToRecord.do
            toast @"Person created" identity
            toast @"Person not created" identity ) # atCase @"Create"
        ( Semigroupoid.do
          indeterminateLinearProgress # action updatePerson
          VariantToRecord.do
            toast @"Person updated" identity
            toast @"Person not updated" identity ) # atCase @"Update"
        ( Semigroupoid.do
          indeterminateLinearProgress # action deletePerson
          VariantToRecord.do
            toast @"Person deleted" identity
            toast @"Person not deleted" identity ) # atCase @"Delete"
    ) # looped
      @( "Filter prefix (surname)" :: String
       , "Name" :: String
       , "Surname" :: String
       , people :: Array { "Name" :: String, "Surname" :: String }
       , selected :: [ none :: {}, picked :: { index :: Int } ]
       )
