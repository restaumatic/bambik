module CrudFluent (crudFluent) where

import Prelude (Unit, identity, (#), ($), (<>), (>>>))

import CrudViewModel (createPerson, deletePerson, entries, loadPeopleCatalogue, peopleLoadedLine, personCreatedLine, personDeletedLine, personLine, personNotCreatedLine, personNotDeletedLine, personNotUpdatedLine, personPickedLine, personUpdatedLine, pick, updatePerson)
import Data.Profunctor.Row.RecordToVariant as RecordToVariant
import Data.Profunctor.Row.VariantToVariant as VariantToVariant
import Data.Variant (match)
import Data.Profunctor.Row.VariantToRecord as VariantToRecord
import Effect (Effect)
import PUI (action, atCase, blankStatus, fold, foreach, joined, looped, subChoice)
import PUI.Web.Fluent (body, button, messageBar, textField)
import PUI.Web (attrWith, clicked, shown, text, (:=))
import PUI.Web.HTML (div, li, ul)
import QualifiedDo.Semigroupoid as Semigroupoid

crudFluent :: Effect Unit
crudFluent =
  body $
    ( Semigroupoid.do
      blankStatus @"People loaded" # action loadPeopleCatalogue
      messageBar @"People loaded" peopleLoadedLine # fold identity
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
          ( VariantToRecord.do
            blankStatus @"Person created"
            blankStatus @"Person not created" ) # action createPerson # atCase @"Create"
          ( VariantToRecord.do
            blankStatus @"Person updated"
            blankStatus @"Person not updated" ) # action updatePerson # atCase @"Update"
          ( VariantToRecord.do
            blankStatus @"Person deleted"
            blankStatus @"Person not deleted" ) # action deletePerson # atCase @"Delete" ) # subChoice
        VariantToRecord.do
          messageBar @"Person picked" personPickedLine # fold pick
          messageBar @"Person created" personCreatedLine # fold identity
          messageBar @"Person not created" personNotCreatedLine # fold identity
          messageBar @"Person updated" personUpdatedLine # fold identity
          messageBar @"Person not updated" personNotUpdatedLine # fold identity
          messageBar @"Person deleted" personDeletedLine # fold identity
          messageBar @"Person not deleted" personNotDeletedLine # fold identity
      ) # looped @( "Filter prefix (surname)" :: String, "Name" :: String, "Surname" :: String, people :: Array { "Name" :: String, "Surname" :: String }, selected :: [ none :: {}, picked :: { index :: Int } ] )
    )

entryFace :: { key :: Int, "Name" :: String, "Surname" :: String, status :: [ selected :: {}, unselected :: {} ] } -> String
entryFace { status } = "padding: 4px 8px; cursor: pointer;" <> match { selected: \_ -> " background: var(--colorBrandBackground2, #cde);", unselected: \_ -> "" } status
