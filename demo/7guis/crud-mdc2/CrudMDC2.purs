module CrudMDC2 (crudMDC2) where

import Prelude (Unit, bind, const, (#), ($), (<<<))

import CrudLogic (createPerson, deletePerson, entries, isSelected, loadPeopleCatalogue, peopleDeleted, pick, refreshPeople, sharedPeopleCatalogue, updatePerson)
import Data.Profunctor.Row.RecordToVariant as RecordToVariant
import Data.Profunctor.Row.VariantToVariant as VariantToVariant
import Data.Variant (match)
import Effect (Effect)
import PUI (action, looped, atCase, toCase, updated, with)
import PUI.Web.HTML (shown, body, text)
import PUI.Web.MDC2 (button, card, cardActions, elevation20, filledTextField, indeterminateLinearProgress, listOf)
import QualifiedDo.Category as Category

crudMDC2 :: Effect Unit
crudMDC2 = do
  catalogue <- sharedPeopleCatalogue
  body $
    elevation20 $
      card $ ( Category.do
          indeterminateLinearProgress @"busy" # action (loadPeopleCatalogue catalogue)
          ( Category.do
              filledTextField @"Filter prefix (surname)" {}
              filledTextField @"Name" {}
              filledTextField @"Surname" {}
              listOf { selected: isSelected } entries (text @"personLine" # shown) # toCase @"picked" _.key # updated (match { picked: pick })
              ( Category.do
                  cardActions $ RecordToVariant.do
                    button @"Create" {}
                    button @"Update" {}
                    button @"Delete" {}
                  VariantToVariant.do
                    indeterminateLinearProgress @"busy" # action (createPerson catalogue) # atCase @"Create"
                    indeterminateLinearProgress @"busy" # action (updatePerson catalogue) # atCase @"Update"
                    indeterminateLinearProgress @"busy" # action (deletePerson catalogue) # atCase @"Delete" ) # updated (match { created: refreshPeople, updated: refreshPeople, deleted: const <<< peopleDeleted })) # looped
      ) # with {}
