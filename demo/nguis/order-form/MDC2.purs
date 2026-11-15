module OrderForm.MDC2 (orderForm) where

import Prelude ((#), ($), Unit)

import Data.Profunctor.Row.RecordToVariant as RecordToVariant
import Data.Profunctor.Row.VariantToRecord as VariantToRecord
import Data.Profunctor.Row.VariantToVariant as VariantToVariant
import Effect (Effect)
import OrderForm (distanceLine, estimateDistance, fulfillmentMode, loadOrder, orderLine, payingLine, printReceipt, setDistance, staleDistanceForgotten, submitOrder, summaryLine)
import PUI (action, captured, foldInto, kept, landed, load, looped, settled, stepped)
import PUI.Web (inCaseAt, provided, staticText, text)
import PUI.Web.MDC2 (acknowledged, body, body1, button', card, filledTextArea, filledTextField, group, headline6, indeterminateLinearProgress, segment, segmentedButton, snackbar, tab, tabBar)
import QualifiedDo.Semigroupoid as Semigroupoid

orderForm :: Effect Unit
orderForm =
  body $ Semigroupoid.do
    ( Semigroupoid.do
      indeterminateLinearProgress # load loadOrder
      snackbar @"Order loaded" captured ) # landed
    ( Semigroupoid.do
      headline6 $ text orderLine
      group @"Identifier" $ Semigroupoid.do
        filledTextField @"Short ID"
        filledTextField @"Unique ID"
      group @"Customer" $ Semigroupoid.do
        filledTextField @"First name"
        filledTextField @"Last name"
      group @"Fulfillment" $ Semigroupoid.do
        tabBar @"Mode" $ VariantToVariant.do
          tab @"Dine in"
          tab @"Takeaway"
          tab @"Delivery"
        filledTextField @"Table" # inCaseAt @"Dine in" @"Mode"
        filledTextField @"Time" # inCaseAt @"Takeaway" @"Mode"
        ( Semigroupoid.do
          filledTextField @"Address" # settled staleDistanceForgotten
          ( body1 $ text distanceLine ) # inCaseAt @"estimated" @"distance" ) # inCaseAt @"Delivery" @"Mode"
      group @"Payment" $ Semigroupoid.do
        filledTextField @"Total"
        segmentedButton @"Method" $ VariantToVariant.do
          segment @"Cash"
          segment @"Card"
        filledTextField @"Paid"
        body1 $ text payingLine
      group @"Kitchen" $ filledTextArea @"Remarks" { columns: 80, rows: 3 }
      card $ body1 $ text summaryLine
      ( Semigroupoid.do
        RecordToVariant.do
          button' @"Estimate distance" { icon: "near_me" } # provided @"Delivery" fulfillmentMode
          button' @"Submit order" { icon: "save" }
          button' @"Receipt" { icon: "file" }
        VariantToRecord.do
          Semigroupoid.do
            indeterminateLinearProgress # action @"Estimate distance" estimateDistance
            foldInto @"Distance estimated" setDistance
          Semigroupoid.do
            indeterminateLinearProgress # action @"Submit order" submitOrder
            acknowledged @"Total missing" $ body1 $ staticText "Enter the order's total before submitting it."
            snackbar @"Order submitted" kept
          Semigroupoid.do
            indeterminateLinearProgress # action @"Receipt" printReceipt
            snackbar @"Receipt printed" kept ) # stepped ) # looped
