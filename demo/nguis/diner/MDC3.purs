module Diner.MDC3 (diner) where

import Prelude ((#), ($), Unit)

import Data.Profunctor.Row.RecordToVariant as RecordToVariant
import Data.Profunctor.Row.VariantToRecord as VariantToRecord
import Data.Profunctor.Row.VariantToVariant as VariantToVariant
import Diner (billLine, bills, cooking, dinnerService, isPlated, itemLine, lineTotalLine, markReady, menuCard, offerLine, orderDish, plateLine, quantityLine, returnUnit, sendToKitchen, settle, tableLine, tableLines, tableState, takingsLine, ticketLine, ticketState)
import Effect (Effect)
import PUI (fold, foldInto, gatheredOf, joined, looped, stepped, with)
import PUI.Web (provided, staticText, text, textAt)
import PUI.Web.HTML (div)
import PUI.Web.MDC3 (body, bodyLarge, bodyMedium, button, button', card, columnHeader, dataCell, dataRow, dataTable, iconButton, list, listItem, listOf, listOfAt', segment, segmentedButton, titleLarge, titleMedium)
import QualifiedDo.Semigroupoid as Semigroupoid

diner :: Effect Unit
diner =
  body $ Semigroupoid.do
    titleLarge $ staticText "Floor"
    segmentedButton @"Table" $ VariantToVariant.do
      segment @"Table 1"
      segment @"Table 2"
      segment @"Table 3"
    ( Semigroupoid.do
      RecordToVariant.do
        listOf @"Dish ordered" @"name" menuCard $ text offerLine
        dataTable "Order"
          ( Semigroupoid.do
            columnHeader "Dish"
            columnHeader "Qty"
            columnHeader "Total"
            columnHeader "" )
          ( ( dataRow $ Semigroupoid.do
            dataCell $ textAt @"dish"
            dataCell $ text quantityLine
            dataCell $ text lineTotalLine
            dataCell $ iconButton @"Unit returned" "remove" ) # gatheredOf @"dish" tableLines )
        Semigroupoid.do
          bodyLarge $ text tableLine
          button' @"Send to kitchen" { icon: "restaurant" } # provided @"taking" tableState
      VariantToRecord.do
        foldInto @"Dish ordered" orderDish
        foldInto @"Unit returned" returnUnit
        fold @"Send to kitchen" sendToKitchen ) # stepped
    titleLarge $ staticText "Kitchen"
    ( Semigroupoid.do
      div $
        ( card $ Semigroupoid.do
          titleMedium $ text ticketLine
          RecordToVariant.do
            ( listOfAt' @"Line plated" @"dish" @"lines" { selected: isPlated } $ text itemLine ) # joined @"Line plated"
            button' @"Order ready" { icon: "room_service" } # provided @"plated" ticketState ) # gatheredOf @"number" cooking
      VariantToRecord.do
        foldInto @"Line plated" plateLine
        foldInto @"Order ready" markReady ) # stepped
    titleLarge $ staticText "Pass"
    bodyMedium $ text takingsLine
    ( Semigroupoid.do
      list $
        ( listItem $ Semigroupoid.do
          text billLine
          button @"Paid" ) # gatheredOf @"number" bills
      foldInto @"Paid" settle ) # stepped
  # looped # with dinnerService
