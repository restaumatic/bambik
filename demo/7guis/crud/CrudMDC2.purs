module CrudMDC2 (crudMDC2) where

import Prelude (Unit, (#), ($))

import CrudViewModel (createPerson, deletePerson, entries, isSelected, loadPeopleCatalogue, personCreatedLine, personDeletedLine, personLine, personNotCreatedLine, personNotDeletedLine, personNotUpdatedLine, personUpdatedLine, pick, updatePerson)
import Data.Profunctor.Row.RecordToVariant as RecordToVariant
import Data.Profunctor.Row.VariantToRecord as VariantToRecord
import Effect (Effect)
import PUI (action, atCase, captured, fold, joined, looped, unshown)
import PUI.Web (text)
import PUI.Web.MDC2 (body, button, filledTextField, indeterminateLinearProgress, listOf', snackbar)
import QualifiedDo.Semigroupoid as Semigroupoid

crudMDC2 :: Effect Unit
crudMDC2 =
  body $ Semigroupoid.do
    indeterminateLinearProgress # action loadPeopleCatalogue
    unshown @"People loaded" # captured
    ( Semigroupoid.do
      filledTextField @"Filter prefix (surname)"
      filledTextField @"Name"
      filledTextField @"Surname"
      RecordToVariant.do
        listOf' @"Person picked" @"key"
          @( key :: Int
           , "Name" :: String
           , "Surname" :: String
           , status :: [ selected :: {}, unselected :: {} ]
           ) { selected: isSelected } entries (text personLine) # joined @"Person picked"
        button @"Create"
        button @"Update"
        button @"Delete"
      VariantToRecord.do
        unshown @"Person picked" # fold pick
        ( Semigroupoid.do
          indeterminateLinearProgress # action createPerson
          VariantToRecord.do
            snackbar @"Person created" personCreatedLine # captured
            snackbar @"Person not created" personNotCreatedLine # captured ) # atCase @"Create"
        ( Semigroupoid.do
          indeterminateLinearProgress # action updatePerson
          VariantToRecord.do
            snackbar @"Person updated" personUpdatedLine # captured
            snackbar @"Person not updated" personNotUpdatedLine # captured ) # atCase @"Update"
        ( Semigroupoid.do
          indeterminateLinearProgress # action deletePerson
          VariantToRecord.do
            snackbar @"Person deleted" personDeletedLine # captured
            snackbar @"Person not deleted" personNotDeletedLine # captured ) # atCase @"Delete"
    ) # looped
      @( "Filter prefix (surname)" :: String
       , "Name" :: String
       , "Surname" :: String
       , people :: Array { "Name" :: String, "Surname" :: String }
       , selected :: [ none :: {}, picked :: { index :: Int } ]
       )
