module Crud.MDC2 (crud) where

import Prelude ((#), ($), Unit)

import Crud (createPerson, deletePerson, entries, filterChanged, freshCatalogue, isSelected, personLine, pick, updatePerson)
import Data.Profunctor.Row.RecordToVariant as RecordToVariant
import Data.Profunctor.Row.VariantToRecord as VariantToRecord
import Effect (Effect)
import PUI (fold, joined, looped, settled, with)
import PUI.Web (providedAt, text)
import PUI.Web.MDC2 (body, button, filledTextField, listOf')
import QualifiedDo.Semigroupoid as Semigroupoid

crud :: Effect Unit
crud =
  body $ Semigroupoid.do
    ( Semigroupoid.do
      filledTextField @"Surname prefix" # settled filterChanged
      filledTextField @"Name"
      filledTextField @"Surname"
    ) # looped
    RecordToVariant.do
      ( listOf' @"Person picked" @"key" { selected: isSelected } entries $ text personLine ) # joined @"Person picked"
      button @"Create"
      button @"Update" # providedAt @"picked" @"selected"
      button @"Delete" # providedAt @"picked" @"selected"
    VariantToRecord.do
      fold @"Person picked" pick
      fold @"Create" createPerson
      fold @"Update" updatePerson
      fold @"Delete" deletePerson
  # looped # with freshCatalogue
