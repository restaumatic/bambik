module CrudHTML (crudHTML) where

import Prelude (identity, (#), ($), (<<<), (<>), (>>>), Unit, bind, const)

import CrudLogic (createPerson, deletePerson, entries, loadPeopleCatalogue, peopleDeleted, pick, refreshPeople, sharedPeopleCatalogue, updatePerson)
import Data.Profunctor.Row.RecordToRecord as RecordToRecord
import Data.Profunctor.Row.RecordToVariant as RecordToVariant
import Data.Profunctor.Row.VariantToVariant as VariantToVariant
import Data.Variant (match)
import Effect (Effect)
import PUI (action, atCase, completed, displayed, field, foreach, looped, pempty, toCase, updated, with)
import PUI.Web.HTML (attrWith, body, button, clicked, div, input, label, li, p, staticText, text, ul, (:=))
import QualifiedDo.Semigroupoid as Semigroupoid

crudHTML :: Effect Unit
crudHTML = do
  catalogue <- sharedPeopleCatalogue
  body $ div $ ( Semigroupoid.do
      pempty # action (loadPeopleCatalogue catalogue)
      ( Semigroupoid.do
          ( RecordToRecord.do
              p ( label $ RecordToRecord.do
                  staticText "Filter prefix (surname) "
                  input "text" # field @"Filter prefix (surname)" )
              p ( label $ RecordToRecord.do
                  staticText "Name "
                  input "text" # field @"Name" )
              p ( label $ RecordToRecord.do
                  staticText "Surname "
                  input "text" # field @"Surname" )) # completed
          ( ul >>> "style" := "list-style: none; margin: 0; padding: 0; border: 1px solid #ccc; max-height: 200px; overflow: auto; width: 100%;" $
              ( clicked ( li >>> attrWith "style" entryFace $ displayed $ RecordToRecord.do
                  text @"Surname"
                  staticText ", "
                  text @"Name" ) ) # foreach @"key" entries) # toCase @"picked" _.key # updated (match { picked: pick })
          ( Semigroupoid.do
              div $ RecordToVariant.do
                button (staticText "Create") # toCase @"create" identity
                button (staticText "Update") # toCase @"update" identity
                button (staticText "Delete") # toCase @"delete" identity
              VariantToVariant.do
                pempty # action (createPerson catalogue) # atCase @"create"
                pempty # action (updatePerson catalogue) # atCase @"update"
                pempty # action (deletePerson catalogue) # atCase @"delete") # updated (match { created: refreshPeople, updated: refreshPeople, deleted: const <<< peopleDeleted })) # looped
  ) # with {}
entryFace :: { "Name" :: String, "Surname" :: String, selected :: Boolean } -> String
entryFace { selected } = entryStyle selected

entryStyle :: Boolean -> String
entryStyle selected = "padding: 4px 8px; cursor: pointer;" <> (if selected then " background: #cde;" else "")
