module OrderForm.MDC2 (orderForm) where

import Prelude ((#), ($), identity, Unit)

import Data.Profunctor.Row.RecordToVariant as RecordToVariant
import Data.Profunctor.Row.VariantToRecord as VariantToRecord
import Data.Profunctor.Row.VariantToVariant as VariantToVariant
import Effect (Effect)
import OrderForm (distanceLine, estimateDistance, fulfillmentMode, loadOrder, orderLine, payingLine, printReceipt, setDistance, staleDistanceForgotten, submitOrder, summaryLine)
import PUI (action, fold, joined, joined2, kept, load, looped, settled)
import PUI.Web (inCaseAt, provided, staticText, text)
import PUI.Web.MDC2 (acknowledged, body, body1, button', card, filledTextArea, filledTextField, group, headline6, indeterminateLinearProgress, segment, segmentedButton, snackbar, tab, tabBar)
import QualifiedDo.Semigroupoid as Semigroupoid

orderForm :: Effect Unit
orderForm =
  body $ Semigroupoid.do
    indeterminateLinearProgress # load loadOrder
    snackbar @"Order loaded" identity
    ( Semigroupoid.do
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
        group @"Kitchen" $ filledTextArea @"Remarks" { columns: 80, rows: 3 } ) # looped
      card $ body1 $ text summaryLine
      RecordToVariant.do
        ( Semigroupoid.do
          button' @"Estimate distance" { icon: "near_me" } # provided @"Delivery" fulfillmentMode
          indeterminateLinearProgress # action @"Estimate distance" estimateDistance ) # joined @"Distance estimated"
        ( Semigroupoid.do
          button' @"Submit order" { icon: "save" }
          indeterminateLinearProgress # action @"Submit order" submitOrder ) # joined2 @"Order submitted" @"Total missing"
        ( Semigroupoid.do
          button' @"Receipt" { icon: "file" }
          indeterminateLinearProgress # action @"Receipt" printReceipt ) # joined @"Receipt printed"
      acknowledged @"Total missing" $ body1 $ staticText "Enter the order's total before submitting it."
      VariantToRecord.do
        fold @"Distance estimated" setDistance
        snackbar @"Order submitted" kept
        snackbar @"Receipt printed" kept ) # looped
