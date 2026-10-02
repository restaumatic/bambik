module OrderFormMDC2 (orderFormMDC2) where

import Prelude (Unit, (#), ($))

import Data.Profunctor.Row.RecordToVariant as RecordToVariant
import Data.Profunctor.Row.VariantToRecord as VariantToRecord
import Data.Variant (match)
import Effect (Effect)
import OrderFormViewModel (distanceLine, distanceOf, estimateDistance, fulfillmentCase, fulfillmentState, loadOrder, orderLine, payingLine, printReceipt, receiptLine, rejectionLine, setDistance, staleDistanceForgotten, submitOrder, submittedLine, summaryLine, summarySettleTime)
import PUI (action, armed, atCase, bracketed, debounced, looped, settled, state, updated, with)
import PUI.Web ((<+>), choice, inCase, shown, shownWhen, text)
import PUI.Web.MDC2 (body, body1, button, card, filledTextArea, filledTextField, group, headline6, indeterminateLinearProgress, segmentedButton, snackbar, tabBar)
import QualifiedDo.Semigroupoid as Semigroupoid

orderFormMDC2 :: Effect Unit
orderFormMDC2 =
  body $ ( Semigroupoid.do
    indeterminateLinearProgress @"Loading order" # action loadOrder
    ( Semigroupoid.do
      ( headline6 $ text orderLine ) # shown
      group @"Identifier" $ Semigroupoid.do
        filledTextField @"Short ID" {}
        filledTextField @"Unique ID" {}
      group @"Customer" $ Semigroupoid.do
        filledTextField @"First name" {}
        filledTextField @"Last name" {}
      group @"Fulfillment" $ Semigroupoid.do
        state @"Mode" @[ "Dine in" :: { "Table" :: String }, "Takeaway" :: { "Time" :: String }, "Delivery" :: { "Address" :: String, distance :: [ estimated :: { km :: Int, to :: String }, unknown :: {} ] } ]
        ( Semigroupoid.do
          state @"distance" @[ estimated :: { km :: Int, to :: String }, unknown :: {} ]
          tabBar @"selected"
            (choice @"Dine in" <+> choice @"Takeaway" <+> choice @"Delivery")
          filledTextField @"Table" {} # inCase @"Dine in" _.selected
          filledTextField @"Time" {} # inCase @"Takeaway" _.selected
          ( Semigroupoid.do
            filledTextField @"Address" {} # settled staleDistanceForgotten
            ( Semigroupoid.do
              button @"Estimate distance" { icon: "near_me" }
              indeterminateLinearProgress @"Estimating distance" # action @[ estimated :: { km :: Int, to :: String } ] estimateDistance # atCase @"Estimate distance" ) # updated (match { estimated: setDistance })
            ( Semigroupoid.do
              state @"km" @Int
              body1 $ text distanceLine ) # shownWhen @"estimated" distanceOf ) # inCase @"Delivery" _.selected ) # bracketed @"Mode" fulfillmentState fulfillmentCase
      group @"Payment" $ Semigroupoid.do
        filledTextField @"Total" {}
        segmentedButton @"Method"
          (choice @"cash" <+> choice @"card")
        filledTextField @"Paid" {}
        ( body1 $ text payingLine ) # shown
      group @"Kitchen" $ filledTextArea @"Remarks" { columns: 80, rows: 3 } ) # looped
    card $ body1 (text summaryLine) # shown # debounced summarySettleTime
    ( RecordToVariant.do
      button @"Submit order" { icon: "save" }
      button @"Receipt" { icon: "file" } ) # armed
    VariantToRecord.do
      Semigroupoid.do
        indeterminateLinearProgress @"Submitting order" # action @[ orderSubmitted :: { "Short ID" :: String } , submissionFailed :: { "Short ID" :: String, reason :: String } ] submitOrder # atCase @"Submit order"
        VariantToRecord.do
          snackbar @"orderSubmitted" submittedLine
          snackbar @"submissionFailed" rejectionLine
      Semigroupoid.do
        indeterminateLinearProgress @"Printing receipt" # action @[ receiptPrinted :: { "Short ID" :: String } ] printReceipt # atCase @"Receipt"
        snackbar @"receiptPrinted" receiptLine
  ) # with {}
