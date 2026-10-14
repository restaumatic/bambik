module Crud.Shoelace (crud) where

import Prelude ((#), ($), (<>), (>>>), identity, Unit)

import Crud (createPerson, deletePerson, entries, loadPeopleCatalogue, personLine, pick, updatePerson)
import Data.Profunctor.Row.RecordToVariant as RecordToVariant
import Data.Variant (match)
import Data.Profunctor.Row.VariantToRecord as VariantToRecord
import Effect (Effect)
import PUI (action, anyOf, atCase, capture, fold, joined, looped)
import PUI.Web (attrWith, clicked, text, (:=))
import PUI.Web.HTML (li, ul)
import PUI.Web.Shoelace (body, button, indeterminateLinearProgress, textField, toast)
import QualifiedDo.Semigroupoid as Semigroupoid

crud :: Effect Unit
crud =
  body $ Semigroupoid.do
    indeterminateLinearProgress # action loadPeopleCatalogue
    capture @"People loaded"
    ( Semigroupoid.do
      textField @"Filter prefix (surname)"
      textField @"Name"
      textField @"Surname"
      RecordToVariant.do
        ( ul >>> "style" := "list-style: none; margin: 0; padding: 0; border: 1px solid var(--sl-color-neutral-300, #ccc); border-radius: 4px; max-height: 200px; overflow: auto; width: 100%;" $
          ( clicked @"Person picked" @"key" $ li >>> attrWith "style" entryFace $ text personLine ) # anyOf @"key"
            @( key :: Int
             , "Name" :: String
             , "Surname" :: String
             , status :: [ selected :: {}, unselected :: {} ]
             ) entries ) # joined
        button @"Create"
        button @"Update"
        button @"Delete"
      VariantToRecord.do
        fold @"Person picked" pick
        ( Semigroupoid.do
          indeterminateLinearProgress # action createPerson
          VariantToRecord.do
            toast @"Person created" identity
            toast @"Person not created" identity ) # atCase @"Create"
        ( Semigroupoid.do
          indeterminateLinearProgress # action updatePerson
          VariantToRecord.do
            toast @"Person updated" identity
            toast @"Person not updated" identity ) # atCase @"Update"
        ( Semigroupoid.do
          indeterminateLinearProgress # action deletePerson
          VariantToRecord.do
            toast @"Person deleted" identity
            toast @"Person not deleted" identity ) # atCase @"Delete"
    ) # looped
      @( "Filter prefix (surname)" :: String
       , "Name" :: String
       , "Surname" :: String
       , people :: Array { "Name" :: String, "Surname" :: String }
       , selected :: [ none :: {}, picked :: { index :: Int } ]
       )

entryFace :: { key :: Int, "Name" :: String, "Surname" :: String, status :: [ selected :: {}, unselected :: {} ] } -> String
entryFace { status } = "padding: 4px 8px; cursor: pointer;" <> match { selected: \_ -> " background: var(--sl-color-primary-100, #cde);", unselected: \_ -> "" } status
