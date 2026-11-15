module Contacts.MDC3 (contacts) where

import Prelude ((#), ($), Unit)

import Contacts (addContact, addressBook, entries, entryLine, isPicked, pick, pickedContact, removePicked, summaryLine)
import Data.Profunctor.Row.RecordToVariant as RecordToVariant
import Data.Profunctor.Row.VariantToRecord as VariantToRecord
import Data.Profunctor.Row.VariantToVariant as VariantToVariant
import Effect (Effect)
import PUI (actedWithin, fold, foldInto, looped, stepped, with)
import PUI.Web (inCaseAt, providedAt, text)
import PUI.Web.HTML (div)
import PUI.Web.MDC3 (body, bodySmall, button', filledTextField, listOf', segment, segmentedButton)
import QualifiedDo.Semigroupoid as Semigroupoid

contacts :: Effect Unit
contacts =
  body $ Semigroupoid.do
    bodySmall $ text summaryLine
    ( Semigroupoid.do
      listOf' @"Contact picked" @"id" { selected: isPicked } entries $ text entryLine
      foldInto @"Contact picked" pick ) # stepped
    div $
      ( Semigroupoid.do
        segmentedButton @"Kind" $ VariantToVariant.do
          segment @"Person"
          segment @"Company"
        filledTextField @"Name"
        filledTextField @"Phone"
        filledTextField @"Tax number" # inCaseAt @"Company" @"Kind" ) # actedWithin @"id" @"contacts" pickedContact
    ( Semigroupoid.do
      RecordToVariant.do
        button' @"New contact" { icon: "person_add" }
        button' @"Delete contact" { icon: "delete" } # providedAt @"picked" @"selected"
      VariantToRecord.do
        fold @"New contact" addContact
        fold @"Delete contact" removePicked ) # stepped
  # looped # with addressBook
