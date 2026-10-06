module CrudMDC3 (crudMDC3) where

import Prelude (Unit, identity, (#), ($))

import CrudViewModel (createPerson, deletePerson, entries, isSelected, loadPeopleCatalogue, personLine, pick, updatePerson)
import Data.Profunctor.Row.RecordToVariant as RecordToVariant
import Data.Profunctor.Row.VariantToVariant as VariantToVariant
import Data.Profunctor.Row.VariantToRecord as VariantToRecord
import Effect (Effect)
import PUI (action, atCase, fold, joined, looped, subChoice, toCase)
import PUI.Web (shown, text)
import PUI.Web.MDC3 (body, button, cardActions, filledTextField, indeterminateLinearProgress, listOf)
import QualifiedDo.Semigroupoid as Semigroupoid

crudMDC3 :: Effect Unit
crudMDC3 =
  body $
    ( Semigroupoid.do
      indeterminateLinearProgress @"Loading people" # action @{ "Filter prefix (surname)" :: String, "Name" :: String, "Surname" :: String, people :: Array { "Name" :: String, "Surname" :: String }, selected :: [ none :: {}, picked :: { index :: Int } ] } loadPeopleCatalogue
      ( Semigroupoid.do
        filledTextField @"Filter prefix (surname)" {}
        filledTextField @"Name" {}
        filledTextField @"Surname" {}
        RecordToVariant.do
          listOf @"picked" @"key" @( key :: Int, "Name" :: String, "Surname" :: String, status :: [ selected :: {}, unselected :: {} ] ) { selected: isSelected } entries (text personLine # shown) # joined @"picked"
          cardActions $ RecordToVariant.do
            button @"Create" {}
            button @"Update" {}
            button @"Delete" {}
        ( VariantToVariant.do
          indeterminateLinearProgress @"Creating person" # action createPerson # atCase @"Create" # toCase @"created" identity
          indeterminateLinearProgress @"Updating person" # action updatePerson # atCase @"Update" # toCase @"updated" identity
          indeterminateLinearProgress @"Deleting person" # action deletePerson # atCase @"Delete" # toCase @"deleted" identity ) # subChoice
        VariantToRecord.do
          fold @"picked" pick
          fold @"created" identity
          fold @"updated" identity
          fold @"deleted" identity
      ) # looped
    )
