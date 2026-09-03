module CrudFluent (crudFluent) where

import Prelude (Unit, bind, const, (#), ($), (<<<), (<>), (>>>))

import CrudLogic (createPerson, deletePerson, entries, loadPeopleCatalogue, peopleDeleted, personLine, pick, refreshPeople, sharedPeopleCatalogue, updatePerson)
import Data.Profunctor.Row.RecordToVariant as RecordToVariant
import Data.Profunctor.Row.VariantToVariant as VariantToVariant
import Data.Variant (match)
import Effect (Effect)
import PUI (action, atCase, foreach, looped, toCase, updated, with, blank)
import PUI.Web.Fluent (button, card, textField)
import PUI.Web.HTML (shown, attrWith, body, clicked, div, li, text, ul, (:=))
import QualifiedDo.Category as Category

crudFluent :: Effect Unit
crudFluent = do
  catalogue <- sharedPeopleCatalogue
  body $
    card $ ( Category.do
        blank # action (loadPeopleCatalogue catalogue)
        ( Category.do
            textField @"Filter prefix (surname)" {}
            textField @"Name" {}
            textField @"Surname" {}
            ( ul >>> "style" := "list-style: none; margin: 0; padding: 0; border: 1px solid var(--colorNeutralStroke1, #ccc); border-radius: 4px; max-height: 200px; overflow: auto; width: 100%;" $
                ( clicked ( li >>> attrWith "style" entryFace $ text personLine # shown ) ) # foreach @"key" entries ) # toCase @"picked" _.key # updated (match { picked: pick })
            ( Category.do
                div $ RecordToVariant.do
                  button @"Create" {}
                  button @"Update" {}
                  button @"Delete" {}
                VariantToVariant.do
                  blank # action (createPerson catalogue) # atCase @"Create"
                  blank # action (updatePerson catalogue) # atCase @"Update"
                  blank # action (deletePerson catalogue) # atCase @"Delete" ) # updated (match { created: refreshPeople, updated: refreshPeople, deleted: const <<< peopleDeleted })) # looped
    ) # with {}
entryFace :: { "Name" :: String, "Surname" :: String, status :: [ selected :: {}, unselected :: {} ] } -> String
entryFace { status } = "padding: 4px 8px; cursor: pointer;" <> match { selected: \_ -> " background: var(--colorBrandBackground2, #cde);", unselected: \_ -> "" } status
