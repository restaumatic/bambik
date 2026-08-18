module CrudShoelace (crudShoelace) where

import Prelude (Unit, bind, const, (#), ($), (<<<), (<>), (>>>))

import CrudLogic (createPerson, deletePerson, entries, loadPeopleCatalogue, peopleDeleted, pick, refreshPeople, sharedPeopleCatalogue, updatePerson)
import Data.Profunctor.Row.RecordToRecord as RecordToRecord
import Data.Profunctor.Row.RecordToVariant as RecordToVariant
import Data.Profunctor.Row.VariantToVariant as VariantToVariant
import Data.Variant (match)
import Effect (Effect)
import PUI (action, atCase, completed, displayed, foreach, looped, pempty, toCase, updated, with)
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
                textField @"Filter prefix (surname)" {}
                textField @"Name" {}
                textField @"Surname" {}) # completed
            ( ul >>> "style" := "list-style: none; margin: 0; padding: 0; border: 1px solid var(--sl-color-neutral-300, #ccc); border-radius: 4px; max-height: 200px; overflow: auto; width: 100%;" $
                ( clicked ( li >>> attrWith "style" entryFace $ displayed $ RecordToRecord.do
                    text @"Surname"
                    staticText ", "
                    text @"Name" ) ) # foreach @"key" entries) # toCase @"picked" _.key # updated (match { picked: pick })
            ( Semigroupoid.do
                div $ RecordToVariant.do
                  button @"Create" {}
                  button @"Update" {}
                  button @"Delete" {}
                VariantToVariant.do
                  pempty # action (createPerson catalogue) # atCase @"Create"
                  pempty # action (updatePerson catalogue) # atCase @"Update"
                  pempty # action (deletePerson catalogue) # atCase @"Delete") # updated (match { created: refreshPeople, updated: refreshPeople, deleted: const <<< peopleDeleted })) # looped
    ) # with {}
entryFace :: { "Name" :: String, "Surname" :: String, selected :: Boolean } -> String
entryFace { selected } = entryStyle selected

entryStyle :: Boolean -> String
entryStyle selected = "padding: 4px 8px; cursor: pointer;" <> (if selected then " background: var(--sl-color-primary-100, #cde);" else "")
