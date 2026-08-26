module CrudHTML (crudHTML) where

import Prelude (identity, (#), ($), (<<<), (<>), (>>>), Unit, bind, const)

import CrudLogic (createPerson, deletePerson, entries, loadPeopleCatalogue, peopleDeleted, pick, refreshPeople, sharedPeopleCatalogue, updatePerson)
import Data.Profunctor.Row.RecordToRecord as RecordToRecord
import Data.Profunctor.Row.RecordToVariant as RecordToVariant
import Data.Profunctor.Row.VariantToVariant as VariantToVariant
import Data.Variant (match)
import Effect (Effect)
import PUI (action, atCase, field, foreach, looped, pempty, toCase, updated, with)
import PUI.Web.HTML (shownAlways, attrWith, body, button, clicked, div, input, label, li, p, staticText, text, ul, (:=))
import QualifiedDo.Semigroupoid as Pipeline

crudHTML :: Effect Unit
crudHTML = do
  catalogue <- sharedPeopleCatalogue
  body $ div $ ( Pipeline.do
      pempty # action (loadPeopleCatalogue catalogue)
      ( Pipeline.do
          p ( label $ Pipeline.do
              (staticText "Filter prefix (surname) ") # shownAlways
              input "text" # field @"Filter prefix (surname)" )
          p ( label $ Pipeline.do
              (staticText "Name ") # shownAlways
              input "text" # field @"Name" )
          p ( label $ Pipeline.do
              (staticText "Surname ") # shownAlways
              input "text" # field @"Surname" )
          ( ul >>> "style" := "list-style: none; margin: 0; padding: 0; border: 1px solid #ccc; max-height: 200px; overflow: auto; width: 100%;" $
              ( clicked ( li >>> attrWith "style" entryFace $ ( RecordToRecord.do
                  text @"Surname"
                  staticText ", "
                  text @"Name" ) # shownAlways ) ) # foreach @"key" entries) # toCase @"picked" _.key # updated (match { picked: pick })
          ( Pipeline.do
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
