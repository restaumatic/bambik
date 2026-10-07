module CrudFluent (crudFluent) where

import Prelude (Unit, identity, (#), ($), (<>), (>>>))

import CrudViewModel (createPerson, deletePerson, entries, loadPeopleCatalogue, personCreatedLine, personDeletedLine, personLine, personPickedLine, personUpdatedLine, pick, updatePerson)
import Data.Profunctor.Row.RecordToVariant as RecordToVariant
import Data.Profunctor.Row.VariantToVariant as VariantToVariant
import Data.Variant (match)
import Data.Profunctor.Row.VariantToRecord as VariantToRecord
import Effect (Effect)
import PUI (action, atCase, blank, fold, foreach, joined, looped, subChoice, toCase)
import PUI.Web.Fluent (body, button, messageBar, textField)
import PUI.Web (attrWith, clicked, shown, text, (:=))
import PUI.Web.HTML (div, li, ul)
import QualifiedDo.Semigroupoid as Semigroupoid

crudFluent :: Effect Unit
crudFluent =
  body $
    ( Semigroupoid.do
      blank # action @{ "Filter prefix (surname)" :: String, "Name" :: String, "Surname" :: String, people :: Array { "Name" :: String, "Surname" :: String }, selected :: [ none :: {}, picked :: { index :: Int } ] } loadPeopleCatalogue
      ( Semigroupoid.do
        textField @"Filter prefix (surname)" {}
        textField @"Name" {}
        textField @"Surname" {}
        RecordToVariant.do
          ( ul >>> "style" := "list-style: none; margin: 0; padding: 0; border: 1px solid var(--colorNeutralStroke1, #ccc); border-radius: 4px; max-height: 200px; overflow: auto; width: 100%;" $
            ( clicked @"Person picked" _.key ( li >>> attrWith "style" entryFace $ text personLine # shown ) ) # foreach @"key" @( key :: Int, "Name" :: String, "Surname" :: String, status :: [ selected :: {}, unselected :: {} ] ) entries ) # joined @"Person picked"
          div $ RecordToVariant.do
            button @"Create" {}
            button @"Update" {}
            button @"Delete" {}
        ( VariantToVariant.do
          blank # action createPerson # atCase @"Create" # toCase @"Person created" identity
          blank # action updatePerson # atCase @"Update" # toCase @"Person updated" identity
          blank # action deletePerson # atCase @"Delete" # toCase @"Person deleted" identity ) # subChoice
        VariantToRecord.do
          messageBar @"Person picked" personPickedLine # fold pick
          messageBar @"Person created" personCreatedLine # fold identity
          messageBar @"Person updated" personUpdatedLine # fold identity
          messageBar @"Person deleted" personDeletedLine # fold identity
      ) # looped
    )

entryFace :: { key :: Int, "Name" :: String, "Surname" :: String, status :: [ selected :: {}, unselected :: {} ] } -> String
entryFace { status } = "padding: 4px 8px; cursor: pointer;" <> match { selected: \_ -> " background: var(--colorBrandBackground2, #cde);", unselected: \_ -> "" } status
