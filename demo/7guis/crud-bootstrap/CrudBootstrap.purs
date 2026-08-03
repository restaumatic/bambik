module CrudBootstrap (crudBootstrap) where

import Prelude (identity, (#), ($), (<<<), Unit, bind, const)

import CrudLogic (createPerson, deletePerson, entries, loadPeopleCatalogue, peopleDeleted, pick, refreshPeople, sharedPeopleCatalogue, updatePerson)
import Data.Profunctor.Row.RecordToRecord as RecordToRecord
import Data.Profunctor.Row.RecordToVariant as RecordToVariant
import Data.Profunctor.Row.VariantToVariant as VariantToVariant
import Data.Variant (match)
import Effect (Effect)
import PUI (action, asCase, asField, completed, displayed, foreach, forField, looped, onCase, silence, toCase, updated, with)
import PUI.Web.Bootstrap (button, card, listGroup, listGroupItem, textField)
import PUI.Web.HTML (body, cl, clWhen, clicked, div, staticText, text, (:=))
import QualifiedDo.Semigroupoid as Semigroupoid

crudBootstrap :: Effect Unit
crudBootstrap = do
  catalogue <- sharedPeopleCatalogue
  body $
    card { caption: "CRUD" } $ ( Semigroupoid.do
        silence # action (loadPeopleCatalogue catalogue)
        ( Semigroupoid.do
            ( RecordToRecord.do
                textField { label: "Filter prefix (surname)" } # asField @"prefix"
                textField { label: "Name" } # asField @"name"
                textField { label: "Surname" } # asField @"surname") # completed
            ( "style" := "max-height: 200px; overflow: auto;" $ listGroup $
                ( clicked ( ( listGroupItem $ displayed $ RecordToRecord.do
                    text # forField @"surname" identity
                    staticText ", "
                    text # forField @"name" identity ) # cl "list-group-item-action" ) # clWhen _.selected "active" ) # foreach @"key" entries) # toCase @"picked" _.key # updated (match { picked: pick })
            ( Semigroupoid.do
                ( div $ RecordToVariant.do
                    button { label: "Create" } # asCase @"create"
                    button { label: "Update" } # asCase @"update"
                    button { label: "Delete" } # asCase @"delete") # cl "d-flex" # cl "gap-2"
                VariantToVariant.do
                  silence # action (createPerson catalogue) # onCase @"create"
                  silence # action (updatePerson catalogue) # onCase @"update"
                  silence # action (deletePerson catalogue) # onCase @"delete") # updated (match { created: refreshPeople, updated: refreshPeople, deleted: const <<< peopleDeleted })) # looped
    ) # with {}
