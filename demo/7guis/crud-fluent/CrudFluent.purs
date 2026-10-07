module CrudFluent (crudFluent) where

import Prelude (Unit, identity, (#), ($), (<>), (>>>))

import CrudViewModel (createPerson, deletePerson, entries, loadPeopleCatalogue, personCreatedLine, personDeletedLine, personLine, personNotCreatedLine, personNotDeletedLine, personNotUpdatedLine, personUpdatedLine, pick, updatePerson)
import Data.Profunctor.Row.RecordToVariant as RecordToVariant
import Data.Variant (match)
import Data.Profunctor.Row.VariantToRecord as VariantToRecord
import Data.Profunctor.Row.VariantToVariant as VariantToVariant
import Effect (Effect)
import PUI (action, atCase, blankStatus, fold, foreach, joined, looped, subChoice)
import PUI.Web.Fluent (body, button, indeterminateLinearProgress, messageBar, textField)
import PUI.Web (attrWith, clicked, shown, text, (:=))
import PUI.Web.HTML (li, ul)
import QualifiedDo.Semigroupoid as Semigroupoid

crudFluent :: Effect Unit
crudFluent =
  body $
    Semigroupoid.do
      indeterminateLinearProgress # action loadPeopleCatalogue
      blankStatus @"People loaded" # fold identity
      ( Semigroupoid.do
        textField @"Filter prefix (surname)" {}
        textField @"Name" {}
        textField @"Surname" {}
        RecordToVariant.do
          ( ul >>> "style" := "list-style: none; margin: 0; padding: 0; border: 1px solid var(--colorNeutralStroke1, #ccc); border-radius: 4px; max-height: 200px; overflow: auto; width: 100%;" $
            ( clicked @"Person picked" _.key ( li >>> attrWith "style" entryFace $ text personLine # shown ) ) # foreach @"key"
              @( key :: Int
               , "Name" :: String
               , "Surname" :: String
               , status :: [ selected :: {}, unselected :: {} ]
               ) entries ) # joined @"Person picked"
          button @"Create" {}
          button @"Update" {}
          button @"Delete" {}
        ( VariantToVariant.do
          indeterminateLinearProgress # action createPerson # atCase @"Create"
          indeterminateLinearProgress # action updatePerson # atCase @"Update"
          indeterminateLinearProgress # action deletePerson # atCase @"Delete" ) # subChoice
        VariantToRecord.do
          blankStatus @"Person picked" # fold pick
          messageBar @"Person created" personCreatedLine # fold identity
          messageBar @"Person not created" personNotCreatedLine # fold identity
          messageBar @"Person updated" personUpdatedLine # fold identity
          messageBar @"Person not updated" personNotUpdatedLine # fold identity
          messageBar @"Person deleted" personDeletedLine # fold identity
          messageBar @"Person not deleted" personNotDeletedLine # fold identity
      ) # looped
        @( "Filter prefix (surname)" :: String
         , "Name" :: String
         , "Surname" :: String
         , people :: Array { "Name" :: String, "Surname" :: String }
         , selected :: [ none :: {}, picked :: { index :: Int } ]
         )

entryFace :: { key :: Int, "Name" :: String, "Surname" :: String, status :: [ selected :: {}, unselected :: {} ] } -> String
entryFace { status } = "padding: 4px 8px; cursor: pointer;" <> match { selected: \_ -> " background: var(--colorBrandBackground2, #cde);", unselected: \_ -> "" } status
