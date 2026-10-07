module CrudMDC3 (crudMDC3) where

import Prelude (Unit, identity, (#), ($))

import CrudViewModel (createPerson, deletePerson, entries, isSelected, loadPeopleCatalogue, peopleLoadedLine, personCreatedLine, personDeletedLine, personLine, personNotCreatedLine, personNotDeletedLine, personNotUpdatedLine, personPickedLine, personUpdatedLine, pick, updatePerson)
import Data.Profunctor.Row.RecordToVariant as RecordToVariant
import Data.Profunctor.Row.VariantToRecord as VariantToRecord
import Effect (Effect)
import PUI (action, atCase, fold, joined, looped)
import PUI.Web (shown, text)
import PUI.Web.MDC3 (body, button, cardActions, filledTextField, indeterminateLinearProgress, listOf, snackbar)
import QualifiedDo.Semigroupoid as Semigroupoid

crudMDC3 :: Effect Unit
crudMDC3 =
  body $
    ( Semigroupoid.do
      indeterminateLinearProgress # action loadPeopleCatalogue
      snackbar @"People loaded" peopleLoadedLine # fold identity
      ( Semigroupoid.do
        filledTextField @"Filter prefix (surname)" {}
        filledTextField @"Name" {}
        filledTextField @"Surname" {}
        RecordToVariant.do
          listOf @"Person picked" @"key" @( key :: Int, "Name" :: String, "Surname" :: String, status :: [ selected :: {}, unselected :: {} ] ) { selected: isSelected } entries (text personLine # shown) # joined @"Person picked"
          cardActions $ RecordToVariant.do
            button @"Create" {}
            button @"Update" {}
            button @"Delete" {}
        VariantToRecord.do
          snackbar @"Person picked" personPickedLine # fold pick
          ( Semigroupoid.do
              indeterminateLinearProgress # action createPerson
              VariantToRecord.do
                snackbar @"Person created" personCreatedLine # fold identity
                snackbar @"Person not created" personNotCreatedLine # fold identity ) # atCase @"Create"
          ( Semigroupoid.do
              indeterminateLinearProgress # action updatePerson
              VariantToRecord.do
                snackbar @"Person updated" personUpdatedLine # fold identity
                snackbar @"Person not updated" personNotUpdatedLine # fold identity ) # atCase @"Update"
          ( Semigroupoid.do
              indeterminateLinearProgress # action deletePerson
              VariantToRecord.do
                snackbar @"Person deleted" personDeletedLine # fold identity
                snackbar @"Person not deleted" personNotDeletedLine # fold identity ) # atCase @"Delete"
      ) # looped @( "Filter prefix (surname)" :: String, "Name" :: String, "Surname" :: String, people :: Array { "Name" :: String, "Surname" :: String }, selected :: [ none :: {}, picked :: { index :: Int } ] )
    )
