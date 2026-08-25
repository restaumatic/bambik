module CrudBootstrap (crudBootstrap) where

import Prelude (Unit, bind, const, (#), ($), (<<<))

import CrudLogic (createPerson, deletePerson, entries, loadPeopleCatalogue, peopleDeleted, pick, refreshPeople, sharedPeopleCatalogue, updatePerson)
import Data.Profunctor.Row.RecordToRecord as RecordToRecord
import Data.Profunctor.Row.RecordToVariant as RecordToVariant
import Data.Profunctor.Row.VariantToVariant as VariantToVariant
import Data.Variant (match)
import Effect (Effect)
import PUI (action, atCase, foreach, looped, pempty, toCase, updated, with)
import PUI.Web.Bootstrap (button, card, listGroup, listGroupItem, textField)
import PUI.Web.HTML (body, cl, clWhen, clicked, div, staticText, text, (:=))
import QualifiedDo.Semigroupoid as Semigroupoid

crudBootstrap :: Effect Unit
crudBootstrap = do
  catalogue <- sharedPeopleCatalogue
  body $
    card $ ( Semigroupoid.do
        pempty # action (loadPeopleCatalogue catalogue)
        ( Semigroupoid.do
            textField @"Filter prefix (surname)" {}
            textField @"Name" {}
            textField @"Surname" {}
            ( "style" := "max-height: 200px; overflow: auto;" $ listGroup $
                ( clicked ( ( listGroupItem $ RecordToRecord.do
                    text @"Surname"
                    staticText ", "
                    text @"Name" ) # cl "list-group-item-action" ) # clWhen _.selected "active" ) # foreach @"key" entries) # toCase @"picked" _.key # updated (match { picked: pick })
            ( Semigroupoid.do
                ( div $ RecordToVariant.do
                    button @"Create" {}
                    button @"Update" {}
                    button @"Delete" {}) # cl "d-flex" # cl "gap-2"
                VariantToVariant.do
                  pempty # action (createPerson catalogue) # atCase @"Create"
                  pempty # action (updatePerson catalogue) # atCase @"Update"
                  pempty # action (deletePerson catalogue) # atCase @"Delete") # updated (match { created: refreshPeople, updated: refreshPeople, deleted: const <<< peopleDeleted })) # looped
    ) # with {}
