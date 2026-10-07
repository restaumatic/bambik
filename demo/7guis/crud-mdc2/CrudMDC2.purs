module CrudMDC2 (crudMDC2) where

import Prelude (Unit, identity, (#), ($))

import CrudViewModel (createPerson, deletePerson, entries, isSelected, loadPeopleCatalogue, personCreatedLine, personDeletedLine, personLine, personPickedLine, personUpdatedLine, pick, updatePerson)
import Data.Profunctor.Row.RecordToVariant as RecordToVariant
import Data.Profunctor.Row.VariantToVariant as VariantToVariant
import Data.Profunctor.Row.VariantToRecord as VariantToRecord
import Effect (Effect)
import PUI (action, atCase, fold, joined, looped, subChoice, toCase)
import PUI.Web (shown, text)
import PUI.Web.MDC2 (body, button, cardActions, filledTextField, indeterminateLinearProgress, listOf, snackbar)
import QualifiedDo.Semigroupoid as Semigroupoid

crudMDC2 :: Effect Unit
crudMDC2 =
  body $
    ( Semigroupoid.do
      indeterminateLinearProgress @"Loading people" # action @{ "Filter prefix (surname)" :: String, "Name" :: String, "Surname" :: String, people :: Array { "Name" :: String, "Surname" :: String }, selected :: [ none :: {}, picked :: { index :: Int } ] } loadPeopleCatalogue
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
        ( VariantToVariant.do
          indeterminateLinearProgress @"Creating person" # action createPerson # atCase @"Create" # toCase @"Person created" identity
          indeterminateLinearProgress @"Updating person" # action updatePerson # atCase @"Update" # toCase @"Person updated" identity
          indeterminateLinearProgress @"Deleting person" # action deletePerson # atCase @"Delete" # toCase @"Person deleted" identity ) # subChoice
        VariantToRecord.do
          snackbar @"Person picked" personPickedLine # fold pick
          snackbar @"Person created" personCreatedLine # fold identity
          snackbar @"Person updated" personUpdatedLine # fold identity
          snackbar @"Person deleted" personDeletedLine # fold identity
      ) # looped
    )
