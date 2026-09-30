module CrudMDC3 (crudMDC3) where

import Prelude (Unit, identity, (#), ($))

import CrudLogic (createPerson, deletePerson, entries, isSelected, loadPeopleCatalogue, peopleDeleted, personLine, pick, refreshPeople, updatePerson)
import Data.Profunctor.Row.RecordToVariant as RecordToVariant
import Data.Profunctor.Row.VariantToVariant as VariantToVariant
import Data.Variant (match)
import Effect (Effect)
import PUI (action, atCase, looped, toCase, updated, with)
import PUI.Web (shown, text)
import PUI.Web.MDC3 (body, button, cardActions, filledTextField, indeterminateLinearProgress, listOf)
import QualifiedDo.Semigroupoid as Semigroupoid

crudMDC3 :: Effect Unit
crudMDC3 =
  body $
    ( Semigroupoid.do
      indeterminateLinearProgress @"Loading people" # action loadPeopleCatalogue
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
            indeterminateLinearProgress @"Creating person" # action createPerson # atCase @"Create" # toCase @"created" identity
            indeterminateLinearProgress @"Updating person" # action updatePerson # atCase @"Update" # toCase @"updated" identity
            indeterminateLinearProgress @"Deleting person" # action deletePerson # atCase @"Delete" # toCase @"deleted" identity ) # updated (match { created: refreshPeople, updated: refreshPeople, deleted: peopleDeleted }) ) # looped
    ) # with {}
