module CrudShoelace (crudShoelace) where

import Prelude (identity, (#), ($), (<<<), (<>), (>>>), Unit, bind, const)

import CrudLogic (createPerson, deletePerson, entries, loadPeopleCatalogue, peopleDeleted, pick, refreshPeople, sharedPeopleCatalogue, updatePerson)
import Data.Profunctor.Row.RecordToRecord as RecordToRecord
import Data.Profunctor.Row.RecordToVariant as RecordToVariant
import Data.Profunctor.Row.VariantToVariant as VariantToVariant
import Data.Variant (match)
import Effect (Effect)
import PUI (action, asCase, asField, atCase, completed, displayed, forField, foreach, looped, pempty, toCase, updated, with)
import PUI.Web.HTML (attrWith, body, clicked, div, li, staticText, text, ul, (:=))
import PUI.Web.Shoelace (button, card, textField)
import QualifiedDo.Semigroupoid as Semigroupoid

crudShoelace :: Effect Unit
crudShoelace = do
  catalogue <- sharedPeopleCatalogue
  body $
    card { caption: "CRUD" } $ ( Semigroupoid.do
        pempty # action (loadPeopleCatalogue catalogue)
        ( Semigroupoid.do
            ( RecordToRecord.do
                textField { label: "Filter prefix (surname)" } # asField @"value" @"prefix"
                textField { label: "Name" } # asField @"value" @"name"
                textField { label: "Surname" } # asField @"value" @"surname") # completed
            ( ul >>> "style" := "list-style: none; margin: 0; padding: 0; border: 1px solid var(--sl-color-neutral-300, #ccc); border-radius: 4px; max-height: 200px; overflow: auto; width: 100%;" $
                ( clicked ( li >>> attrWith "style" entryFace $ displayed $ RecordToRecord.do
                    text # forField @"value" @"surname" identity
                    staticText ", "
                    text # forField @"value" @"name" identity ) ) # foreach @"key" entries) # toCase @"picked" _.key # updated (match { picked: pick })
            ( Semigroupoid.do
                div $ RecordToVariant.do
                  button { label: "Create" } # asCase @"clicked" @"create"
                  button { label: "Update" } # asCase @"clicked" @"update"
                  button { label: "Delete" } # asCase @"clicked" @"delete"
                VariantToVariant.do
                  pempty # action (createPerson catalogue) # atCase @"create"
                  pempty # action (updatePerson catalogue) # atCase @"update"
                  pempty # action (deletePerson catalogue) # atCase @"delete") # updated (match { created: refreshPeople, updated: refreshPeople, deleted: const <<< peopleDeleted })) # looped
    ) # with {}

-- closed signature states the clicked content's row (the row-stating
-- exception): the merge reads name/surname, the style reads selected
entryFace :: { name :: String, surname :: String, selected :: Boolean } -> String
entryFace { selected } = entryStyle selected

entryStyle :: Boolean -> String
entryStyle selected = "padding: 4px 8px; cursor: pointer;" <> (if selected then " background: var(--sl-color-primary-100, #cde);" else "")
