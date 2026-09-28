module CrudMDC3 (crudMDC3) where

import Prelude (Unit, bind, const, (#), ($), (<<<))

import CrudLogic (createPerson, deletePerson, entries, isSelected, loadPeopleCatalogue, peopleDeleted, personLine, pick, refreshPeople, sharedPeopleCatalogue, updatePerson)
import Data.Profunctor.Row.RecordToVariant as RecordToVariant
import Data.Profunctor.Row.VariantToVariant as VariantToVariant
import Data.Variant (match)
import Effect (Effect)
import PUI (action, looped, atCase, updated, with)
import PUI.Web (shown, text)
import PUI.Web.MDC3 (body, button, cardActions, filledTextField, indeterminateLinearProgress, listOf)
import QualifiedDo.Semigroupoid as Semigroupoid

crudMDC3 :: Effect Unit
crudMDC3 = do
  catalogue <- sharedPeopleCatalogue
  body $
    ( Semigroupoid.do
      indeterminateLinearProgress @"busy" # action (loadPeopleCatalogue catalogue)
      ( Semigroupoid.do
        filledTextField @"Filter prefix (surname)" {}
        filledTextField @"Name" {}
        filledTextField @"Surname" {}
        listOf @"picked" @"key" { selected: isSelected } entries (text personLine # shown) # updated (match { picked: pick })
        ( Semigroupoid.do
          cardActions $ RecordToVariant.do
            button @"Create" {}
            button @"Update" {}
            button @"Delete" {}
          VariantToVariant.do
            indeterminateLinearProgress @"busy" # action (createPerson catalogue) # atCase @"Create"
            indeterminateLinearProgress @"busy" # action (updatePerson catalogue) # atCase @"Update"
            indeterminateLinearProgress @"busy" # action (deletePerson catalogue) # atCase @"Delete" ) # updated (match { created: refreshPeople, updated: refreshPeople, deleted: const <<< peopleDeleted })) # looped
    ) # with {}
