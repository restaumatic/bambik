module CrudShoelace (crudShoelace) where

import Prelude (Unit, identity, (#), ($), (<>), (>>>))

import CrudViewModel (createPerson, deletePerson, entries, loadPeopleCatalogue, peopleLoadedLine, personCreatedLine, personDeletedLine, personLine, personNotCreatedLine, personNotDeletedLine, personNotUpdatedLine, personPickedLine, personUpdatedLine, pick, updatePerson)
import Data.Profunctor.Row.RecordToVariant as RecordToVariant
import Data.Variant (match)
import Data.Profunctor.Row.VariantToRecord as VariantToRecord
import Effect (Effect)
import PUI (action, atCase, fold, foreach, joined, looped)
import PUI.Web (attrWith, clicked, shown, text, (:=))
import PUI.Web.HTML (div, li, ul)
import PUI.Web.Shoelace (body, button, indeterminateLinearProgress, textField, toast)
import QualifiedDo.Semigroupoid as Semigroupoid

crudShoelace :: Effect Unit
crudShoelace =
  body $
    ( Semigroupoid.do
      indeterminateLinearProgress # action loadPeopleCatalogue
      toast @"People loaded" peopleLoadedLine # fold identity
      ( Semigroupoid.do
        textField @"Filter prefix (surname)" {}
        textField @"Name" {}
        textField @"Surname" {}
        RecordToVariant.do
          ( ul >>> "style" := "list-style: none; margin: 0; padding: 0; border: 1px solid var(--sl-color-neutral-300, #ccc); border-radius: 4px; max-height: 200px; overflow: auto; width: 100%;" $
            ( clicked @"Person picked" _.key ( li >>> attrWith "style" entryFace $ text personLine # shown ) ) # foreach @"key" @( key :: Int, "Name" :: String, "Surname" :: String, status :: [ selected :: {}, unselected :: {} ] ) entries ) # joined @"Person picked"
          div $ RecordToVariant.do
            button @"Create" {}
            button @"Update" {}
            button @"Delete" {}
        VariantToRecord.do
          toast @"Person picked" personPickedLine # fold pick
          ( Semigroupoid.do
              indeterminateLinearProgress # action createPerson
              VariantToRecord.do
                toast @"Person created" personCreatedLine # fold identity
                toast @"Person not created" personNotCreatedLine # fold identity ) # atCase @"Create"
          ( Semigroupoid.do
              indeterminateLinearProgress # action updatePerson
              VariantToRecord.do
                toast @"Person updated" personUpdatedLine # fold identity
                toast @"Person not updated" personNotUpdatedLine # fold identity ) # atCase @"Update"
          ( Semigroupoid.do
              indeterminateLinearProgress # action deletePerson
              VariantToRecord.do
                toast @"Person deleted" personDeletedLine # fold identity
                toast @"Person not deleted" personNotDeletedLine # fold identity ) # atCase @"Delete"
      ) # looped @( "Filter prefix (surname)" :: String, "Name" :: String, "Surname" :: String, people :: Array { "Name" :: String, "Surname" :: String }, selected :: [ none :: {}, picked :: { index :: Int } ] )
    )

entryFace :: { key :: Int, "Name" :: String, "Surname" :: String, status :: [ selected :: {}, unselected :: {} ] } -> String
entryFace { status } = "padding: 4px 8px; cursor: pointer;" <> match { selected: \_ -> " background: var(--sl-color-primary-100, #cde);", unselected: \_ -> "" } status
