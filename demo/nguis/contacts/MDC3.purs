module Contacts.MDC3 (contacts) where

import Prelude ((#), ($), Unit)

import Contacts (entries, entryLine, filed, isPicked, pickedContact, summaryLine)
import Data.Profunctor.Row.RecordToVariant as RecordToVariant
import Data.Profunctor.Row.VariantToVariant as VariantToVariant
import Effect (Effect)
import PUI (actedWithin, edits, joined)
import PUI.Web (inCaseAt, providedAt, text)
import PUI.Web.HTML (div)
import PUI.Web.MDC3 (body, bodySmall, button', filledTextField, listOf', segment, segmentedButton)
import QualifiedDo.Semigroupoid as Semigroupoid

contacts :: Effect Unit
contacts =
  body $ Semigroupoid.do
    bodySmall $ text summaryLine
    RecordToVariant.do
      ( listOf' @"Contact picked" @"id" { selected: isPicked } entries $ text entryLine ) # joined @"Contact picked"
      div $
        ( Semigroupoid.do
          segmentedButton @"Kind" $ VariantToVariant.do
            segment @"Person"
            segment @"Company"
          filledTextField @"Name"
          filledTextField @"Phone"
          filledTextField @"Tax number" # inCaseAt @"Company" @"Kind" ) # actedWithin @"id" @"contacts" pickedContact # edits @"Contact edited"
      button' @"New contact" { icon: "person_add" }
      button' @"Delete contact" { icon: "delete" } # providedAt @"picked" @"selected"
  # filed
