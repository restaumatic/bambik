module CrudShoelace (crudShoelace) where

import Prelude (Unit, identity, (#), ($), (<>), (>>>))

import CrudViewModel (createPerson, deletePerson, entries, loadPeopleCatalogue, peopleDeleted, personLine, pick, refreshPeople, updatePerson)
import Data.Profunctor.Row.RecordToVariant as RecordToVariant
import Data.Profunctor.Row.VariantToVariant as VariantToVariant
import Data.Variant (match)
import Effect (Effect)
import PUI (action, atCase, blank, foreach, looped, state, toCase, updated, with)
import PUI.Web (attrWith, clicked, shown, text, (:=))
import PUI.Web.HTML (div, li, ul)
import PUI.Web.Shoelace (body, button, textField)
import QualifiedDo.Semigroupoid as Semigroupoid

crudShoelace :: Effect Unit
crudShoelace =
  body $
    ( Semigroupoid.do
      blank # action loadPeopleCatalogue
      ( Semigroupoid.do
        state @"people" @(Array { "Name" :: String, "Surname" :: String })
        state @"selected" @[ picked :: { index :: Int }, none :: {} ]
        textField @"Filter prefix (surname)" {}
        textField @"Name" {}
        textField @"Surname" {}
        ( ul >>> "style" := "list-style: none; margin: 0; padding: 0; border: 1px solid var(--sl-color-neutral-300, #ccc); border-radius: 4px; max-height: 200px; overflow: auto; width: 100%;" $
          ( clicked @"picked" _.key ( Semigroupoid.do
            state @"Name" @String
            state @"Surname" @String
            state @"status" @[ selected :: {}, unselected :: {} ]
            li >>> attrWith "style" entryFace $ text personLine # shown ) ) # foreach @"key" @Int entries ) # updated (match { picked: pick })
        ( Semigroupoid.do
          div $ RecordToVariant.do
            button @"Create" {}
            button @"Update" {}
            button @"Delete" {}
          VariantToVariant.do
            blank # action @(Array { "Name" :: String, "Surname" :: String }) createPerson # atCase @"Create" # toCase @"created" identity
            blank # action @(Array { "Name" :: String, "Surname" :: String }) updatePerson # atCase @"Update" # toCase @"updated" identity
            blank # action @(Array { "Name" :: String, "Surname" :: String }) deletePerson # atCase @"Delete" # toCase @"deleted" identity ) # updated (match { created: refreshPeople, updated: refreshPeople, deleted: peopleDeleted }) ) # looped
    ) # with {}

entryFace :: forall r1. { "Name" :: String, "Surname" :: String, status :: [ selected :: {}, unselected :: {} ] | r1 } -> String
entryFace { status } = "padding: 4px 8px; cursor: pointer;" <> match { selected: \_ -> " background: var(--sl-color-primary-100, #cde);", unselected: \_ -> "" } status
