module CrudHTML (crudHTML) where

import Prelude (Unit, identity, (#), ($), (<>), (>>>))

import CrudViewModel (createPerson, deletePerson, entries, loadPeopleCatalogue, personCreatedLine, personDeletedLine, personLine, personPickedLine, personUpdatedLine, pick, updatePerson)
import Data.Profunctor.Row.RecordToVariant as RecordToVariant
import Data.Profunctor.Row.VariantToVariant as VariantToVariant
import Data.Variant (match)
import Data.Profunctor.Row.VariantToRecord as VariantToRecord
import Effect (Effect)
import PUI (action, atCase, blank, fold, foreach, joined, looped, subChoice, toCase)
import PUI.Web (attrWith, clicked, shown, staticText, text, (:=))
import PUI.Web.HTML (body, button, div, input, label, li, output, p, ul)
import QualifiedDo.Semigroupoid as Semigroupoid

crudHTML :: Effect Unit
crudHTML =
  body $ div $ ( Semigroupoid.do
    blank # action @{ "Filter prefix (surname)" :: String, "Name" :: String, "Surname" :: String, people :: Array { "Name" :: String, "Surname" :: String }, selected :: [ none :: {}, picked :: { index :: Int } ] } loadPeopleCatalogue
    ( Semigroupoid.do
      p ( label $ Semigroupoid.do
        (staticText @"Filter prefix (surname) ") # shown
        input @"Filter prefix (surname)" "text" )
      p ( label $ Semigroupoid.do
        (staticText @"Name ") # shown
        input @"Name" "text" )
      p ( label $ Semigroupoid.do
        (staticText @"Surname ") # shown
        input @"Surname" "text" )
      RecordToVariant.do
        ( ul >>> "style" := "list-style: none; margin: 0; padding: 0; border: 1px solid #ccc; max-height: 200px; overflow: auto; width: 100%;" $
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
        output @"Person picked" personPickedLine # fold pick
        output @"Person created" personCreatedLine # fold identity
        output @"Person updated" personUpdatedLine # fold identity
        output @"Person deleted" personDeletedLine # fold identity
    ) # looped
  )

entryFace :: { key :: Int, "Name" :: String, "Surname" :: String, status :: [ selected :: {}, unselected :: {} ] } -> String
entryFace { status } = "padding: 4px 8px; cursor: pointer;" <> match { selected: \_ -> " background: #cde;", unselected: \_ -> "" } status
