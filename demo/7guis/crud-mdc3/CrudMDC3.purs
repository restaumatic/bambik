module CrudMDC3 (crudMDC3) where

import Prelude (identity, (#), ($), (<<<), Unit, bind, const)

import CrudLogic (createPerson, deletePerson, entries, loadPeopleCatalogue, peopleDeleted, pick, refreshPeople, sharedPeopleCatalogue, updatePerson)
import Data.Profunctor.Row.RecordToRecord as RecordToRecord
import Data.Profunctor.Row.RecordToVariant as RecordToVariant
import Data.Profunctor.Row.VariantToVariant as VariantToVariant
import Data.Variant (match)
import Effect (Effect)
import PUI (action, completed, displayed, looped, atCase, toCase, updated, with)
import PUI.Web.HTML (body, staticText, text)
import PUI.Web.MDC3 (button, card, cardActions, elevation5, filledTextField, indeterminateLinearProgress, listOf)
import QualifiedDo.Semigroupoid as Semigroupoid

crudMDC3 :: Effect Unit
crudMDC3 = do
  catalogue <- sharedPeopleCatalogue
  body $
    elevation5 $
      card { caption: "CRUD" } $ ( Semigroupoid.do
          indeterminateLinearProgress @"busy" # action (loadPeopleCatalogue catalogue)
          ( Semigroupoid.do
              ( RecordToRecord.do
                  filledTextField @"prefix" { floatingLabel: "Filter prefix (surname)" }
                  filledTextField @"name" {}
                  filledTextField @"surname" {}) # completed
              listOf { selected: _.selected } entries ( displayed $ RecordToRecord.do
                  text @"surname"
                  staticText ", "
                  text @"name" ) # toCase @"picked" _.key # updated (match { picked: pick })
              ( Semigroupoid.do
                  cardActions $ RecordToVariant.do
                    button @"create" { label: "Create" }
                    button @"update" { label: "Update" }
                    button @"delete" { label: "Delete" }
                  VariantToVariant.do
                    indeterminateLinearProgress @"busy" # action (createPerson catalogue) # atCase @"create"
                    indeterminateLinearProgress @"busy" # action (updatePerson catalogue) # atCase @"update"
                    indeterminateLinearProgress @"busy" # action (deletePerson catalogue) # atCase @"delete") # updated (match { created: refreshPeople, updated: refreshPeople, deleted: const <<< peopleDeleted })) # looped
      ) # with {}
