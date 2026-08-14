module CrudBootstrap (crudBootstrap) where

import Prelude (identity, (#), ($), (<<<), Unit, bind, const)

import CrudLogic (createPerson, deletePerson, entries, loadPeopleCatalogue, peopleDeleted, pick, refreshPeople, sharedPeopleCatalogue, updatePerson)
import Data.Profunctor.Row.RecordToRecord as RecordToRecord
import Data.Profunctor.Row.RecordToVariant as RecordToVariant
import Data.Profunctor.Row.VariantToVariant as VariantToVariant
import Data.Variant (match)
import Effect (Effect)
import PUI (action, asCase, atCase, completed, foreach, looped, pempty, toCase, updated, with)
import PUI.Web.Bootstrap (button, card, listGroup, listGroupItem, textField)
import PUI.Web.HTML (body, cl, clWhen, clicked, div, staticText, text, (:=))
import QualifiedDo.Semigroupoid as Semigroupoid

crudBootstrap :: Effect Unit
crudBootstrap = do
  catalogue <- sharedPeopleCatalogue
  body $
    card { caption: "CRUD" } $ ( Semigroupoid.do
        pempty # action (loadPeopleCatalogue catalogue)
        ( Semigroupoid.do
            ( RecordToRecord.do
                textField @"prefix" { label: "Filter prefix (surname)" }
                textField @"name" {}
                textField @"surname" {}) # completed
            ( "style" := "max-height: 200px; overflow: auto;" $ listGroup $
                ( clicked ( ( listGroupItem $ RecordToRecord.do
                    text @"surname"
                    staticText ", "
                    text @"name" ) # cl "list-group-item-action" ) # clWhen _.selected "active" ) # foreach @"key" entries) # toCase @"picked" _.key # updated (match { picked: pick })
            ( Semigroupoid.do
                ( div $ RecordToVariant.do
                    button { label: "Create" } # asCase @"clicked" @"create"
                    button { label: "Update" } # asCase @"clicked" @"update"
                    button { label: "Delete" } # asCase @"clicked" @"delete") # cl "d-flex" # cl "gap-2"
                VariantToVariant.do
                  pempty # action (createPerson catalogue) # atCase @"create"
                  pempty # action (updatePerson catalogue) # atCase @"update"
                  pempty # action (deletePerson catalogue) # atCase @"delete") # updated (match { created: refreshPeople, updated: refreshPeople, deleted: const <<< peopleDeleted })) # looped
    ) # with {}
