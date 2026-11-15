module Crud.MDC2 (crud) where

import Prelude ((#), ($), Unit)

import Crud (catalogued, entries, isSelected, personLine)
import Data.Profunctor.Row.RecordToVariant as RecordToVariant
import Effect (Effect)
import PUI (edits, joined)
import PUI.Web (providedAt, text)
import PUI.Web.MDC2 (body, button, filledTextField, listOf')
import QualifiedDo.Semigroupoid as Semigroupoid

crud :: Effect Unit
crud =
  body $ RecordToVariant.do
    filledTextField @"Surname prefix" # edits @"Prefix changed"
    ( Semigroupoid.do
      filledTextField @"Name"
      filledTextField @"Surname" ) # edits @"Entry edited"
    ( listOf' @"Person picked" @"key" { selected: isSelected } entries $ text personLine ) # joined @"Person picked"
    button @"Create"
    button @"Update" # providedAt @"picked" @"selected"
    button @"Delete" # providedAt @"picked" @"selected"
  # catalogued
