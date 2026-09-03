module CrudHTML (crudHTML) where

import Prelude (identity, (#), ($), (<<<), (<>), (>>>), Unit, bind, const)

import CrudLogic (createPerson, deletePerson, entries, loadPeopleCatalogue, peopleDeleted, personLine, pick, refreshPeople, sharedPeopleCatalogue, updatePerson)
import Data.Profunctor.Row.RecordToVariant as RecordToVariant
import Data.Profunctor.Row.VariantToVariant as VariantToVariant
import Data.Variant (match)
import Effect (Effect)
import PUI (action, atCase, field, foreach, looped, toCase, updated, with, blank)
import PUI.Web.HTML (shown, attrWith, body, button, clicked, div, input, label, li, p, staticText, text, ul, (:=))
import QualifiedDo.Category as Category

crudHTML :: Effect Unit
crudHTML = do
  catalogue <- sharedPeopleCatalogue
  body $ div $ ( Category.do
      blank # action (loadPeopleCatalogue catalogue)
      ( Category.do
          p ( label $ Category.do
              (staticText "Filter prefix (surname) ") # shown
              input "text" # field @"Filter prefix (surname)" )
          p ( label $ Category.do
              (staticText "Name ") # shown
              input "text" # field @"Name" )
          p ( label $ Category.do
              (staticText "Surname ") # shown
              input "text" # field @"Surname" )
          ( ul >>> "style" := "list-style: none; margin: 0; padding: 0; border: 1px solid #ccc; max-height: 200px; overflow: auto; width: 100%;" $
              ( clicked ( li >>> attrWith "style" entryFace $ text personLine # shown ) ) # foreach @"key" entries ) # toCase @"picked" _.key # updated (match { picked: pick })
          ( Category.do
              div $ RecordToVariant.do
                button (staticText "Create") # toCase @"create" identity
                button (staticText "Update") # toCase @"update" identity
                button (staticText "Delete") # toCase @"delete" identity
              VariantToVariant.do
                blank # action (createPerson catalogue) # atCase @"create"
                blank # action (updatePerson catalogue) # atCase @"update"
                blank # action (deletePerson catalogue) # atCase @"delete" ) # updated (match { created: refreshPeople, updated: refreshPeople, deleted: const <<< peopleDeleted })) # looped
  ) # with {}
entryFace :: { "Name" :: String, "Surname" :: String, status :: [ selected :: {}, unselected :: {} ] } -> String
entryFace { status } = "padding: 4px 8px; cursor: pointer;" <> match { selected: \_ -> " background: #cde;", unselected: \_ -> "" } status
