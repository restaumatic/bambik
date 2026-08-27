module CrudBootstrap (crudBootstrap) where

import Prelude (Unit, bind, const, (#), ($), (<<<))

import CrudLogic (createPerson, deletePerson, entries, isSelected, loadPeopleCatalogue, peopleDeleted, pick, refreshPeople, sharedPeopleCatalogue, updatePerson)
import Data.Profunctor.Row.RecordToRecord as RecordToRecord
import Data.Profunctor.Row.RecordToVariant as RecordToVariant
import Data.Profunctor.Row.VariantToVariant as VariantToVariant
import Data.Variant (match)
import Effect (Effect)
import PUI (action, atCase, foreach, looped, toCase, updated, with, blank)
import PUI.Web.Bootstrap (button, card, listGroup, listGroupItem, textField)
import PUI.Web.HTML (body, cl, clWhen, clicked, div, staticText, text, (:=))
import QualifiedDo.Category as Category

crudBootstrap :: Effect Unit
crudBootstrap = do
  catalogue <- sharedPeopleCatalogue
  body $
    card $ ( Category.do
        blank # action (loadPeopleCatalogue catalogue)
        ( Category.do
            textField @"Filter prefix (surname)" {}
            textField @"Name" {}
            textField @"Surname" {}
            ( "style" := "max-height: 200px; overflow: auto;" $ listGroup $
                ( clicked ( ( listGroupItem $ RecordToRecord.do
                    text @"Surname"
                    staticText ", "
                    text @"Name" ) # cl "list-group-item-action" ) # clWhen isSelected "active" ) # foreach @"key" entries ) # toCase @"picked" _.key # updated (match { picked: pick })
            ( Category.do
                ( div $ RecordToVariant.do
                    button @"Create" {}
                    button @"Update" {}
                    button @"Delete" {} ) # cl "d-flex" # cl "gap-2"
                VariantToVariant.do
                  blank # action (createPerson catalogue) # atCase @"Create"
                  blank # action (updatePerson catalogue) # atCase @"Update"
                  blank # action (deletePerson catalogue) # atCase @"Delete" ) # updated (match { created: refreshPeople, updated: refreshPeople, deleted: const <<< peopleDeleted })) # looped
    ) # with {}
