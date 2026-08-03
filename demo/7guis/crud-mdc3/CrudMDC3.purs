module CrudMDC3 (crudMDC3) where

import Prelude (identity, (#), ($), (<<<), Unit, bind, const)

import CrudLogic (createPerson, deletePerson, entries, loadPeopleCatalogue, peopleDeleted, pick, refreshPeople, sharedPeopleCatalogue, updatePerson)
import Data.Profunctor.Row.RecordToRecord as RecordToRecord
import Data.Profunctor.Row.RecordToVariant as RecordToVariant
import Data.Profunctor.Row.VariantToVariant as VariantToVariant
import Data.Variant (match)
import Effect (Effect)
import PUI (action, asCase, asField, completed, displayed, forField, looped, onCase, toCase, updated, with)
import PUI.Web.HTML (body, staticText, text)
import PUI.Web.MDC3 (button, card, cardActions, elevation5, filledTextField, indeterminateLinearProgress, listOf)
import QualifiedDo.Semigroupoid as Semigroupoid

crudMDC3 :: Effect Unit
crudMDC3 = do
  catalogue <- sharedPeopleCatalogue
  body $
    elevation5 $
      card { caption: "CRUD" } $ ( Semigroupoid.do
          indeterminateLinearProgress # action (loadPeopleCatalogue catalogue)
          ( Semigroupoid.do
              ( RecordToRecord.do
                  filledTextField { floatingLabel: "Filter prefix (surname)" } # asField @"prefix"
                  filledTextField { floatingLabel: "Name" } # asField @"name"
                  filledTextField { floatingLabel: "Surname" } # asField @"surname") # completed
              listOf { selected: _.selected } entries ( displayed $ RecordToRecord.do
                  text # forField @"surname" identity
                  staticText ", "
                  text # forField @"name" identity ) # toCase @"picked" _.key # updated (match { picked: pick })
              ( Semigroupoid.do
                  cardActions $ RecordToVariant.do
                    button { label: "Create" } # asCase @"create"
                    button { label: "Update" } # asCase @"update"
                    button { label: "Delete" } # asCase @"delete"
                  VariantToVariant.do
                    indeterminateLinearProgress # action (createPerson catalogue) # onCase @"create"
                    indeterminateLinearProgress # action (updatePerson catalogue) # onCase @"update"
                    indeterminateLinearProgress # action (deletePerson catalogue) # onCase @"delete") # updated (match { created: refreshPeople, updated: refreshPeople, deleted: const <<< peopleDeleted })) # looped
      ) # with {}
