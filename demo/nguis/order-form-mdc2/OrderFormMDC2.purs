module OrderFormMDC2 (orderFormMDC2) where

import Prelude (Unit, (#), ($))

import Data.Profunctor.Row.RecordToVariant as RecordToVariant
import Data.Profunctor.Row.VariantToRecord as VariantToRecord
import Data.Profunctor.Row.VariantToVariant as VariantToVariant
import Data.Variant (match)
import Effect (Effect)
import OrderFormLogic (awayLine, deliveryDistance, deliveryLine, dineInLine, distanceLine, distanceOf, estimateDistance, fulfillmentCase, fulfillmentOf, fulfillmentState, loadOrder, orderLine, paidLine, payingLine, printReceipt, receiptLine, rejectionLine, selection, setDistance, staleDistanceForgotten, submitOrder, submittedLine, summaryLine, summarySettleTime, takeawayLine)
import PUI (action, armed, atCase, bracketed, debounced, looped, settled, updated, with)
import PUI.Web (choice, inCase, shown, shownWhen, text)
import PUI.Web.MDC2 (body, body1, button, card, filledTextArea, filledTextField, group, headline6, indeterminateLinearProgress, segmentedButton, snackbar, tabBar)
import QualifiedDo.Semigroupoid as Semigroupoid

orderFormMDC2 :: Effect Unit
orderFormMDC2 =
  body $ ( Semigroupoid.do
    indeterminateLinearProgress @"busy" # action loadOrder
    ( Semigroupoid.do
      ( headline6 $ text orderLine ) # shown
      group @"Identifier" $ Semigroupoid.do
        filledTextField @"Short ID" {}
        filledTextField @"Unique ID" {}
      group @"Customer" $ Semigroupoid.do
        filledTextField @"First name" {}
        filledTextField @"Last name" {}
      group @"Fulfillment" $
        ( Semigroupoid.do
          tabBar @"selected"
            [ choice @"Dine in", choice @"Takeaway", choice @"Delivery" ]
          filledTextField @"Table" {} # inCase @"Dine in" selection
          filledTextField @"Time" {} # inCase @"Takeaway" selection
          ( Semigroupoid.do
            filledTextField @"Address" {} # settled staleDistanceForgotten
            ( Semigroupoid.do
              button @"Estimate distance" { icon: "near_me" }
              indeterminateLinearProgress @"busy" # action estimateDistance # atCase @"Estimate distance" ) # updated (match { estimated: setDistance })
            ( body1 $ text distanceLine ) # shownWhen @"estimated" distanceOf ) # inCase @"Delivery" selection ) # bracketed @"Mode" fulfillmentState fulfillmentCase
      group @"Payment" $ Semigroupoid.do
        filledTextField @"Total" {}
        segmentedButton @"Method"
          [ choice @"cash", choice @"card" ]
        filledTextField @"Paid" {}
        ( body1 $ text payingLine ) # shown
      group @"Kitchen" $ filledTextArea @"Remarks" { columns: 80, rows: 3 } ) # looped
    card $ body1 ( Semigroupoid.do
      text summaryLine # shown # debounced summarySettleTime
      text dineInLine # shownWhen @"Dine in" fulfillmentOf
      text takeawayLine # shownWhen @"Takeaway" fulfillmentOf
      text deliveryLine # shownWhen @"Delivery" fulfillmentOf
      text awayLine # shownWhen @"estimated" deliveryDistance
      text paidLine # shown # debounced summarySettleTime )
    ( RecordToVariant.do
      button @"Submit order" { icon: "save" }
      button @"Receipt" { icon: "file" } ) # armed
    VariantToVariant.do
      indeterminateLinearProgress @"busy" # action submitOrder # atCase @"Submit order"
      indeterminateLinearProgress @"busy" # action printReceipt # atCase @"Receipt"
    VariantToRecord.do
      snackbar @"orderSubmitted" submittedLine
      snackbar @"submissionFailed" rejectionLine
      snackbar @"receiptPrinted" receiptLine
  ) # with {}
