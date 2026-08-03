module CrudShoelace (crudShoelace) where

import Prelude (identity, (#), ($), (<<<), (<>), (>>>), Unit, bind, const)

import CrudLogic (createPerson, deletePerson, entries, loadPeopleCatalogue, peopleDeleted, pick, refreshPeople, sharedPeopleCatalogue, updatePerson)
import Data.Profunctor.Row.RecordToRecord as RecordToRecord
import Data.Profunctor.Row.RecordToVariant as RecordToVariant
import Data.Profunctor.Row.VariantToVariant as VariantToVariant
import Data.Variant (match)
import Effect (Effect)
import PUI (action, asCase, asField, completed, displayed, foreach, forField, looped, onCase, silence, toCase, updated, with)
import PUI.Web.HTML (attrWith, body, clicked, div, li, staticText, text, ul, (:=))
import PUI.Web.Shoelace (button, card, textField)
import QualifiedDo.Semigroupoid as Semigroupoid

crudShoelace :: Effect Unit
crudShoelace = do
  catalogue <- sharedPeopleCatalogue
  body $
    card { caption: "CRUD" } $ ( Semigroupoid.do
        silence # action (loadPeopleCatalogue catalogue)
        ( Semigroupoid.do
            ( RecordToRecord.do
                textField { label: "Filter prefix (surname)" } # asField @"prefix"
                textField { label: "Name" } # asField @"name"
                textField { label: "Surname" } # asField @"surname") # completed
            ( ul >>> "style" := "list-style: none; margin: 0; padding: 0; border: 1px solid var(--sl-color-neutral-300, #ccc); border-radius: 4px; max-height: 200px; overflow: auto; width: 100%;" $
                ( clicked ( li >>> attrWith "style" (entryStyle <<< _.selected) $ displayed $ RecordToRecord.do
                    text # forField @"surname" identity
                    staticText ", "
                    text # forField @"name" identity ) ) # foreach @"key" entries) # toCase @"picked" _.key # updated (match { picked: pick })
            ( Semigroupoid.do
                div $ RecordToVariant.do
                  button { label: "Create" } # asCase @"create"
                  button { label: "Update" } # asCase @"update"
                  button { label: "Delete" } # asCase @"delete"
                VariantToVariant.do
                  silence # action (createPerson catalogue) # onCase @"create"
                  silence # action (updatePerson catalogue) # onCase @"update"
                  silence # action (deletePerson catalogue) # onCase @"delete") # updated (match { created: refreshPeople, updated: refreshPeople, deleted: const <<< peopleDeleted })) # looped
    ) # with {}

entryStyle :: Boolean -> String
entryStyle selected = "padding: 4px 8px; cursor: pointer;" <> (if selected then " background: var(--sl-color-primary-100, #cde);" else "")
