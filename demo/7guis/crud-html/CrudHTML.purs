module CrudHTML (crudHTML) where

import Prelude (identity, (#), ($), (<<<), (<>), (>>>), Unit, bind, const)

import CrudLogic (createPerson, deletePerson, entries, loadPeopleCatalogue, peopleDeleted, pick, refreshPeople, sharedPeopleCatalogue, updatePerson)
import Data.Profunctor.Row.RecordToRecord as RecordToRecord
import Data.Profunctor.Row.RecordToVariant as RecordToVariant
import Data.Profunctor.Row.VariantToVariant as VariantToVariant
import Data.Variant (match)
import Effect (Effect)
import PUI (action, asField, completed, displayed, field, foreach, forField, looped, onCase, silence, toCase, updated, with)
import PUI.Web.HTML (attrWith, body, button, clicked, div, input, label, li, p, staticText, text, ul, (:=))
import QualifiedDo.Semigroupoid as Semigroupoid

crudHTML :: Effect Unit
crudHTML = do
  catalogue <- sharedPeopleCatalogue
  body $ div $ ( Semigroupoid.do
      silence # action (loadPeopleCatalogue catalogue)
      ( Semigroupoid.do
          ( RecordToRecord.do
              p ( label $ RecordToRecord.do
                  staticText "Filter prefix (surname) "
                  input "text" # field @"value" ) # asField @"prefix"
              p ( label $ RecordToRecord.do
                  staticText "Name "
                  input "text" # field @"value" ) # asField @"name"
              p ( label $ RecordToRecord.do
                  staticText "Surname "
                  input "text" # field @"value" ) # asField @"surname") # completed
          ( ul >>> "style" := "list-style: none; margin: 0; padding: 0; border: 1px solid #ccc; max-height: 200px; overflow: auto; width: 100%;" $
              ( clicked ( li >>> attrWith "style" (entryStyle <<< _.selected) $ displayed $ RecordToRecord.do
                  text # forField @"surname" identity
                  staticText ", "
                  text # forField @"name" identity ) ) # foreach @"key" entries) # toCase @"picked" _.key # updated (match { picked: pick })
          ( Semigroupoid.do
              div $ RecordToVariant.do
                button (staticText "Create") # toCase @"create" identity
                button (staticText "Update") # toCase @"update" identity
                button (staticText "Delete") # toCase @"delete" identity
              VariantToVariant.do
                silence # action (createPerson catalogue) # onCase @"create"
                silence # action (updatePerson catalogue) # onCase @"update"
                silence # action (deletePerson catalogue) # onCase @"delete") # updated (match { created: refreshPeople, updated: refreshPeople, deleted: const <<< peopleDeleted })) # looped
  ) # with {}

entryStyle :: Boolean -> String
entryStyle selected = "padding: 4px 8px; cursor: pointer;" <> (if selected then " background: #cde;" else "")
