module Diner.MDC2 (diner) where

import Prelude ((#), ($), Unit)

import Data.Profunctor.Row.RecordToVariant as RecordToVariant
import Data.Profunctor.Row.VariantToVariant as VariantToVariant
import Diner (billLine, bills, cooking, isPlated, itemLine, lineTotalLine, menuCard, offerLine, opened, quantityLine, tableLine, tableLines, tableState, takingsLine, ticketLine, ticketState)
import Effect (Effect)
import PUI (type (+), edits, gatheredOf, joined)
import PUI.Web (provided, staticText, text, textAt)
import PUI.Web.HTML (div)
import PUI.Web.MDC2 (body, body1, body2, button, button', card, columnHeader, dataCell, dataRow, dataTable, headline6, iconButton, list, listItem, listOf, listOfAt', segment, segmentedButton, subtitle1)
import QualifiedDo.Semigroupoid as Semigroupoid

diner :: Effect Unit
diner =
  body $ RecordToVariant.do
    Semigroupoid.do
      headline6 $ staticText "Floor"
      RecordToVariant.do
        ( segmentedButton @"Table" $ VariantToVariant.do
          segment @"Table 1"
          segment @"Table 2"
          segment @"Table 3" ) # edits @"Table chosen"
        ( listOf @"Dish ordered" @"name" menuCard $ text offerLine ) # joined @"Dish ordered"
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
            dataCell $ iconButton @"Unit returned" "remove" ) # gatheredOf @"dish" tableLines ) # joined @"Unit returned"
        Semigroupoid.do
          body1 $ text tableLine
          button' @"Send to kitchen" { icon: "restaurant" } # provided @"taking" tableState
    Semigroupoid.do
      headline6 $ staticText "Kitchen"
      ( div $
        ( card $ Semigroupoid.do
          subtitle1 $ text ticketLine
          RecordToVariant.do
            ( listOfAt' @"Line plated" @"dish" @"lines" { selected: isPlated } $ text itemLine ) # joined @"Line plated"
            button' @"Order ready" { icon: "room_service" } # provided @"plated" ticketState ) # gatheredOf @"number" cooking ) # joined @("Line plated" + "Order ready")
    Semigroupoid.do
      headline6 $ staticText "Pass"
      body2 $ text takingsLine
      ( list $
        ( listItem $ Semigroupoid.do
          text billLine
          button @"Paid" ) # gatheredOf @"number" bills ) # joined @"Paid"
  # opened
