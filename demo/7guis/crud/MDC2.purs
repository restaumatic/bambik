module Crud.MDC2 (crud) where

import Prelude ((#), ($), identity, Unit)

import Crud (createPerson, deletePerson, entries, isSelected, loadPeopleCatalogue, personLine, pick, updatePerson)
import Data.Profunctor.Row.RecordToVariant as RecordToVariant
import Data.Profunctor.Row.VariantToRecord as VariantToRecord
import Effect (Effect)
import PUI (action, atCase, capture, fold, joined, looped)
import PUI.Web (text)
import PUI.Web.MDC2 (body, button, filledTextField, indeterminateLinearProgress, listOf', snackbar)
import QualifiedDo.Semigroupoid as Semigroupoid

crud :: Effect Unit
crud =
  body $ Semigroupoid.do
    indeterminateLinearProgress # action loadPeopleCatalogue
    capture @"People loaded"
    ( Semigroupoid.do
      filledTextField @"Filter prefix (surname)"
      filledTextField @"Name"
      filledTextField @"Surname"
      RecordToVariant.do
        ( listOf' @"Person picked" @"key"
          @( key :: Int
           , "Name" :: String
           , "Surname" :: String
           , status :: [ selected :: {}, unselected :: {} ]
           ) { selected: isSelected } entries $ text personLine ) # joined @"Person picked"
        button @"Create"
        button @"Update"
        button @"Delete"
      VariantToRecord.do
        fold @"Person picked" pick
        ( Semigroupoid.do
          indeterminateLinearProgress # action createPerson
          VariantToRecord.do
            snackbar @"Person created" identity
            snackbar @"Person not created" identity ) # atCase @"Create"
        ( Semigroupoid.do
          indeterminateLinearProgress # action updatePerson
          VariantToRecord.do
            snackbar @"Person updated" identity
            snackbar @"Person not updated" identity ) # atCase @"Update"
        ( Semigroupoid.do
          indeterminateLinearProgress # action deletePerson
          VariantToRecord.do
            snackbar @"Person deleted" identity
            snackbar @"Person not deleted" identity ) # atCase @"Delete"
    ) # looped
      @( "Filter prefix (surname)" :: String
       , "Name" :: String
       , "Surname" :: String
       , people :: Array { "Name" :: String, "Surname" :: String }
       , selected :: [ none :: {}, picked :: { index :: Int } ]
       )
