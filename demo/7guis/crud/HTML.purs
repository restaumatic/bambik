module Crud.HTML (crud) where

import Prelude (Unit, (#), ($), (<>), (>>>))

import Crud (createPerson, deletePerson, entries, loadPeopleCatalogue, personCreatedLine, personDeletedLine, personLine, personNotCreatedLine, personNotDeletedLine, personNotUpdatedLine, personUpdatedLine, pick, updatePerson)
import Data.Profunctor.Row.RecordToVariant as RecordToVariant
import Data.Variant (match)
import Data.Profunctor.Row.VariantToRecord as VariantToRecord
import Effect (Effect)
import PUI (action, anyOf, atCase, captured, fold, joined, looped, unshown)
import PUI.Web (attrWith, clicked, staticText, text, (:=))
import PUI.Web.HTML (body, button, div, indeterminateLinearProgress, input, label, li, output, p, ul)
import QualifiedDo.Semigroupoid as Semigroupoid

crud :: Effect Unit
crud =
  body $ div $ Semigroupoid.do
    indeterminateLinearProgress # action loadPeopleCatalogue
    unshown @"People loaded" # captured
    ( Semigroupoid.do
      p ( label $ Semigroupoid.do
        staticText "Filter prefix (surname) "
        input @"Filter prefix (surname)" "text" )
      p ( label $ Semigroupoid.do
        staticText "Name "
        input @"Name" "text" )
      p ( label $ Semigroupoid.do
        staticText "Surname "
        input @"Surname" "text" )
      RecordToVariant.do
        ( ul >>> "style" := "list-style: none; margin: 0; padding: 0; border: 1px solid #ccc; max-height: 200px; overflow: auto; width: 100%;" $
          ( clicked @"Person picked" @"key" ( li >>> attrWith "style" entryFace $ text personLine ) ) # anyOf @"key"
            @( key :: Int
             , "Name" :: String
             , "Surname" :: String
             , status :: [ selected :: {}, unselected :: {} ]
             ) entries ) # joined @"Person picked"
        button @"Create"
        button @"Update"
        button @"Delete"
      VariantToRecord.do
        unshown @"Person picked" # fold pick
        ( Semigroupoid.do
          indeterminateLinearProgress # action createPerson
          VariantToRecord.do
            output @"Person created" personCreatedLine # captured
            output @"Person not created" personNotCreatedLine # captured ) # atCase @"Create"
        ( Semigroupoid.do
          indeterminateLinearProgress # action updatePerson
          VariantToRecord.do
            output @"Person updated" personUpdatedLine # captured
            output @"Person not updated" personNotUpdatedLine # captured ) # atCase @"Update"
        ( Semigroupoid.do
          indeterminateLinearProgress # action deletePerson
          VariantToRecord.do
            output @"Person deleted" personDeletedLine # captured
            output @"Person not deleted" personNotDeletedLine # captured ) # atCase @"Delete"
    ) # looped
      @( "Filter prefix (surname)" :: String
       , "Name" :: String
       , "Surname" :: String
       , people :: Array { "Name" :: String, "Surname" :: String }
       , selected :: [ none :: {}, picked :: { index :: Int } ]
       )

entryFace :: { key :: Int, "Name" :: String, "Surname" :: String, status :: [ selected :: {}, unselected :: {} ] } -> String
entryFace { status } = "padding: 4px 8px; cursor: pointer;" <> match { selected: \_ -> " background: #cde;", unselected: \_ -> "" } status
