module CrudMDC3 (crudMDC3) where

import Prelude (Unit, bind, const, (#), ($), (<<<))

import CrudLogic (createPerson, deletePerson, entries, loadPeopleCatalogue, peopleDeleted, pick, refreshPeople, sharedPeopleCatalogue, updatePerson)
import Data.Profunctor.Row.RecordToRecord as RecordToRecord
import Data.Profunctor.Row.RecordToVariant as RecordToVariant
import Data.Profunctor.Row.VariantToVariant as VariantToVariant
import Data.Variant (match)
import Effect (Effect)
import PUI (action, completed, tapped, looped, atCase, toCase, updated, with)
import PUI.Web.HTML (body, staticText, text)
import PUI.Web.MDC3 (button, card, cardActions, elevation5, filledTextField, indeterminateLinearProgress, listOf)
import QualifiedDo.Semigroupoid as Semigroupoid

crudMDC3 :: Effect Unit
crudMDC3 = do
  catalogue <- sharedPeopleCatalogue
  body $
    elevation5 $
      card $ ( Semigroupoid.do
          indeterminateLinearProgress @"busy" # action (loadPeopleCatalogue catalogue)
          ( Semigroupoid.do
              ( RecordToRecord.do
                  filledTextField @"Filter prefix (surname)" {}
                  filledTextField @"Name" {}
                  filledTextField @"Surname" {}) # completed
              listOf { selected: _.selected } entries ( tapped $ RecordToRecord.do
                  text @"Surname"
                  staticText ", "
                  text @"Name" ) # toCase @"picked" _.key # updated (match { picked: pick })
              ( Semigroupoid.do
                  cardActions $ RecordToVariant.do
                    button @"Create" {}
                    button @"Update" {}
                    button @"Delete" {}
                  VariantToVariant.do
                    indeterminateLinearProgress @"busy" # action (createPerson catalogue) # atCase @"Create"
                    indeterminateLinearProgress @"busy" # action (updatePerson catalogue) # atCase @"Update"
                    indeterminateLinearProgress @"busy" # action (deletePerson catalogue) # atCase @"Delete") # updated (match { created: refreshPeople, updated: refreshPeople, deleted: const <<< peopleDeleted })) # looped
      ) # with {}
