module CrudBootstrap (crudBootstrap) where

import Prelude (Unit, bind, const, (#), ($), (<<<), (>>>))

import CrudLogic (createPerson, deletePerson, entries, isSelected, loadPeopleCatalogue, peopleDeleted, personLine, pick, refreshPeople, sharedPeopleCatalogue, updatePerson)
import Data.Profunctor.Row.RecordToVariant as RecordToVariant
import Data.Profunctor.Row.VariantToVariant as VariantToVariant
import Data.Variant (match)
import Effect (Effect)
import PUI (action, atCase, foreach, looped, toCase, updated, with, blank)
import PUI.Web.Bootstrap (body, button, card, listGroup, listGroupItem, textField)
import PUI.Web.HTML (cl, clWhen, clicked, div, text, (:=))
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
        ( cl "overflow-auto" >>> "style" := "max-height: 200px;" $ listGroup $
          ( clicked ( ( listGroupItem $ text personLine ) # cl "list-group-item-action" ) # clWhen isSelected "active" ) # foreach @"key" entries ) # toCase @"picked" _.key # updated (match { picked: pick })
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
