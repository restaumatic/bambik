module CrudFluent (crudFluent) where

import Prelude (identity, (#), ($), (<<<), (<>), (>>>), Unit, bind, const)

import CrudLogic (createPerson, deletePerson, entries, loadPeopleCatalogue, peopleDeleted, pick, refreshPeople, sharedPeopleCatalogue, updatePerson)
import Data.Profunctor.Row.RecordToRecord as RecordToRecord
import Data.Profunctor.Row.RecordToVariant as RecordToVariant
import Data.Profunctor.Row.VariantToVariant as VariantToVariant
import Data.Variant (match)
import Effect (Effect)
import PUI (action, atCase, completed, displayed, foreach, looped, pempty, toCase, updated, with)
import PUI.Web.Fluent (button, card, textField)
import PUI.Web.HTML (attrWith, body, clicked, div, li, staticText, text, ul, (:=))
import QualifiedDo.Semigroupoid as Semigroupoid

crudFluent :: Effect Unit
crudFluent = do
  catalogue <- sharedPeopleCatalogue
  body $
    card { caption: "CRUD" } $ ( Semigroupoid.do
        pempty # action (loadPeopleCatalogue catalogue)
        ( Semigroupoid.do
            ( RecordToRecord.do
                textField @"prefix" { label: "Filter prefix (surname)" }
                textField @"name" {}
                textField @"surname" {}) # completed
            ( ul >>> "style" := "list-style: none; margin: 0; padding: 0; border: 1px solid var(--colorNeutralStroke1, #ccc); border-radius: 4px; max-height: 200px; overflow: auto; width: 100%;" $
                ( clicked ( li >>> attrWith "style" entryFace $ displayed $ RecordToRecord.do
                    text @"surname"
                    staticText ", "
                    text @"name" ) ) # foreach @"key" entries) # toCase @"picked" _.key # updated (match { picked: pick })
            ( Semigroupoid.do
                div $ RecordToVariant.do
                  button @"create" { label: "Create" }
                  button @"update" { label: "Update" }
                  button @"delete" { label: "Delete" }
                VariantToVariant.do
                  pempty # action (createPerson catalogue) # atCase @"create"
                  pempty # action (updatePerson catalogue) # atCase @"update"
                  pempty # action (deletePerson catalogue) # atCase @"delete") # updated (match { created: refreshPeople, updated: refreshPeople, deleted: const <<< peopleDeleted })) # looped
    ) # with {}
entryFace :: { name :: String, surname :: String, selected :: Boolean } -> String
entryFace { selected } = entryStyle selected

entryStyle :: Boolean -> String
entryStyle selected = "padding: 4px 8px; cursor: pointer;" <> (if selected then " background: var(--colorBrandBackground2, #cde);" else "")
