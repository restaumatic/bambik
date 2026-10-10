module CrudHTML (crudHTML) where

import Prelude (Unit, identity, (#), ($), (<>), (>>>))

import CrudViewModel (createPerson, deletePerson, entries, loadPeopleCatalogue, personCreatedLine, personDeletedLine, personLine, personNotCreatedLine, personNotDeletedLine, personNotUpdatedLine, personUpdatedLine, pick, updatePerson)
import Data.Profunctor.Row.RecordToVariant as RecordToVariant
import Data.Variant (match)
import Data.Profunctor.Row.VariantToRecord as VariantToRecord
import Data.Profunctor.Row.VariantToVariant as VariantToVariant
import Effect (Effect)
import PUI (action, atCase, blankStatus, fold, foreach, joined, looped, subChoice)
import PUI.Web (attrWith, clicked, shown, staticText, text, (:=))
import PUI.Web.HTML (body, button, div, indeterminateLinearProgress, input, label, li, output, p, ul)
import QualifiedDo.Semigroupoid as Semigroupoid

crudHTML :: Effect Unit
crudHTML =
  body $ div $ Semigroupoid.do
    indeterminateLinearProgress # action loadPeopleCatalogue
    blankStatus @"People loaded" # fold identity
    ( Semigroupoid.do
      p ( label $ Semigroupoid.do
        (staticText "Filter prefix (surname) ") # shown
        input @"Filter prefix (surname)" "text" )
      p ( label $ Semigroupoid.do
        (staticText "Name ") # shown
        input @"Name" "text" )
      p ( label $ Semigroupoid.do
        (staticText "Surname ") # shown
        input @"Surname" "text" )
      RecordToVariant.do
        ( ul >>> "style" := "list-style: none; margin: 0; padding: 0; border: 1px solid #ccc; max-height: 200px; overflow: auto; width: 100%;" $
          ( clicked @"Person picked" @"key" ( li >>> attrWith "style" entryFace $ text personLine # shown ) ) # foreach @"key"
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
        output @"Person created" personCreatedLine # fold identity
        output @"Person not created" personNotCreatedLine # fold identity
        output @"Person updated" personUpdatedLine # fold identity
        output @"Person not updated" personNotUpdatedLine # fold identity
        output @"Person deleted" personDeletedLine # fold identity
        output @"Person not deleted" personNotDeletedLine # fold identity
    ) # looped
      @( "Filter prefix (surname)" :: String
       , "Name" :: String
       , "Surname" :: String
       , people :: Array { "Name" :: String, "Surname" :: String }
       , selected :: [ none :: {}, picked :: { index :: Int } ]
       )

entryFace :: { key :: Int, "Name" :: String, "Surname" :: String, status :: [ selected :: {}, unselected :: {} ] } -> String
entryFace { status } = "padding: 4px 8px; cursor: pointer;" <> match { selected: \_ -> " background: #cde;", unselected: \_ -> "" } status
