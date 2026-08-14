module CrudMDC2 (crudMDC2) where

import Prelude (identity, (#), ($), (<<<), Unit, bind, const)

import CrudLogic (createPerson, deletePerson, entries, loadPeopleCatalogue, peopleDeleted, pick, refreshPeople, sharedPeopleCatalogue, updatePerson)
import Data.Profunctor.Row.RecordToRecord as RecordToRecord
import Data.Profunctor.Row.RecordToVariant as RecordToVariant
import Data.Profunctor.Row.VariantToVariant as VariantToVariant
import Data.Variant (match)
import Effect (Effect)
import PUI (action, asCase, asField, completed, displayed, forField, looped, atCase, toCase, updated, with)
import PUI.Web.HTML (body, staticText, text)
import PUI.Web.MDC2 (button, card, cardActions, elevation20, filledTextField, indeterminateLinearProgress, listOf)
import QualifiedDo.Semigroupoid as Semigroupoid

crudMDC2 :: Effect Unit
crudMDC2 = do
  catalogue <- sharedPeopleCatalogue
  body $
    elevation20 $
      card { caption: "CRUD" } $ ( Semigroupoid.do
          indeterminateLinearProgress # action (loadPeopleCatalogue catalogue)
          ( Semigroupoid.do
              ( RecordToRecord.do
                  filledTextField { floatingLabel: "Filter prefix (surname)" } # asField @"value" @"prefix"
                  filledTextField { floatingLabel: "Name" } # asField @"value" @"name"
                  filledTextField { floatingLabel: "Surname" } # asField @"value" @"surname") # completed
              listOf { selected: _.selected } entries ( displayed $ RecordToRecord.do
                  text # forField @"surname" identity
                  staticText ", "
                  text # forField @"name" identity ) # toCase @"picked" _.key # updated (match { picked: pick })
              ( Semigroupoid.do
                  cardActions $ RecordToVariant.do
                    button { label: "Create" } # asCase @"clicked" @"create"
                    button { label: "Update" } # asCase @"clicked" @"update"
                    button { label: "Delete" } # asCase @"clicked" @"delete"
                  VariantToVariant.do
                    indeterminateLinearProgress # action (createPerson catalogue) # atCase @"create"
                    indeterminateLinearProgress # action (updatePerson catalogue) # atCase @"update"
                    indeterminateLinearProgress # action (deletePerson catalogue) # atCase @"delete") # updated (match { created: refreshPeople, updated: refreshPeople, deleted: const <<< peopleDeleted })) # looped
      ) # with {}
