module CrudMDC3 (crudMDC3) where

import Prelude (Unit, identity, (#), ($))

import CrudViewModel (createPerson, deletePerson, entries, isSelected, loadPeopleCatalogue, personCreatedLine, personDeletedLine, personLine, personNotCreatedLine, personNotDeletedLine, personNotUpdatedLine, personUpdatedLine, pick, updatePerson)
import Data.Profunctor.Row.RecordToVariant as RecordToVariant
import Data.Profunctor.Row.VariantToRecord as VariantToRecord
import Data.Profunctor.Row.VariantToVariant as VariantToVariant
import Effect (Effect)
import PUI (action, atCase, blankStatus, fold, joined, looped, subChoice)
import PUI.Web (shown, text)
import PUI.Web.MDC3 (body, button, filledTextField, indeterminateLinearProgress, listOf, snackbar)
import QualifiedDo.Semigroupoid as Semigroupoid

crudMDC3 :: Effect Unit
crudMDC3 =
  body $ Semigroupoid.do
    indeterminateLinearProgress # action loadPeopleCatalogue
    blankStatus @"People loaded" # fold identity
    ( Semigroupoid.do
      filledTextField @"Filter prefix (surname)" {}
      filledTextField @"Name" {}
      filledTextField @"Surname" {}
      RecordToVariant.do
        listOf @"Person picked" @"key"
          @( key :: Int
           , "Name" :: String
           , "Surname" :: String
           , status :: [ selected :: {}, unselected :: {} ]
           ) { selected: isSelected } entries (text personLine # shown) # joined @"Person picked"
        button @"Create" {}
        button @"Update" {}
        button @"Delete" {}
      ( VariantToVariant.do
        indeterminateLinearProgress # action createPerson # atCase @"Create"
        indeterminateLinearProgress # action updatePerson # atCase @"Update"
        indeterminateLinearProgress # action deletePerson # atCase @"Delete" ) # subChoice
      VariantToRecord.do
        blankStatus @"Person picked" # fold pick
        snackbar @"Person created" personCreatedLine # fold identity
        snackbar @"Person not created" personNotCreatedLine # fold identity
        snackbar @"Person updated" personUpdatedLine # fold identity
        snackbar @"Person not updated" personNotUpdatedLine # fold identity
        snackbar @"Person deleted" personDeletedLine # fold identity
        snackbar @"Person not deleted" personNotDeletedLine # fold identity
    ) # looped
      @( "Filter prefix (surname)" :: String
       , "Name" :: String
       , "Surname" :: String
       , people :: Array { "Name" :: String, "Surname" :: String }
       , selected :: [ none :: {}, picked :: { index :: Int } ]
       )
