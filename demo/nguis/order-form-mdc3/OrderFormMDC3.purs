module OrderFormMDC3 (orderFormMDC3) where

import Prelude (identity, Unit, (#), ($))

import Data.Profunctor.Row.RecordToVariant as RecordToVariant
import Data.Profunctor.Row.VariantToRecord as VariantToRecord
import Data.Profunctor.Row.VariantToVariant as VariantToVariant
import Effect (Effect)
import OrderFormViewModel (distanceLine, distanceOf, estimateDistance, fulfillmentCase, fulfillmentState, loadOrder, orderLine, payingLine, printReceipt, receiptLine, rejectionLine, setDistance, staleDistanceForgotten, submitOrder, submittedLine, summaryLine, summarySettleTime)
import PUI (PUI, action, atCase, blankStatus, bracketed, debounced, fold, looped, prefixed, settled, subStrong, suffixed, updated)
import PUI.Web (Web, (<+>), choice, inCaseAt, pane, text)
import PUI.Web.MDC3 (body, bodyLarge, button, card, filledTextArea, filledTextField, group, headlineSmall, indeterminateLinearProgress, segmentedButton, snackbar, tabBar)
import QualifiedDo.Semigroupoid as Semigroupoid

orderFormMDC3 :: Effect Unit
orderFormMDC3 =
  body $ Semigroupoid.do
    indeterminateLinearProgress # action loadOrder
    blankStatus @"Order loaded" # fold identity
    ( Semigroupoid.do
      prefixed (headlineSmall $ text orderLine) $ group @"Identifier" $ Semigroupoid.do
        filledTextField @"Short ID" {}
        filledTextField @"Unique ID" {}
      group @"Customer" $ Semigroupoid.do
        filledTextField @"First name" {}
        filledTextField @"Last name" {}
      group @"Fulfillment" $
        ( Semigroupoid.do
          tabBar @"selected"
            (choice @"Dine in" <+> choice @"Takeaway" <+> choice @"Delivery")
          filledTextField @"Table" {} # inCaseAt @"Dine in" @"selected"
          filledTextField @"Time" {} # inCaseAt @"Takeaway" @"selected"
          distanceEstimate # subStrong # inCaseAt @"Delivery" @"selected" ) # bracketed @"Mode"
              @( "Dine in" :: { "Table" :: String }
               , "Takeaway" :: { "Time" :: String }
               , "Delivery" :: { "Address" :: String, distance :: [ estimated :: { km :: Int, to :: String }, unknown :: {} ] }
               )
              @( selected :: [ "Dine in" :: {}, "Takeaway" :: {}, "Delivery" :: {} ]
               , "Table" :: String
               , "Time" :: String
               , "Address" :: String
               , distance :: [ estimated :: { km :: Int, to :: String }, unknown :: {} ]
               ) fulfillmentState fulfillmentCase
      group @"Payment" $ Semigroupoid.do
        filledTextField @"Total" {}
        segmentedButton @"Method"
          (choice @"cash" <+> choice @"card")
        filledTextField @"Paid" {} # suffixed (bodyLarge $ text payingLine)
      group @"Kitchen" $ filledTextArea @"Remarks" { columns: 80, rows: 3 } ) # looped
        @( "Identifier" :: { "Short ID" :: String, "Unique ID" :: String }
         , "Customer" :: { "First name" :: String, "Last name" :: String }
         , "Fulfillment" :: { "Mode" :: [ "Dine in" :: { "Table" :: String }, "Takeaway" :: { "Time" :: String }, "Delivery" :: { "Address" :: String, distance :: [ estimated :: { km :: Int, to :: String }, unknown :: {} ] } ] }
         , "Payment" :: { "Total" :: String, "Method" :: [ "cash" :: {}, "card" :: {} ], "Paid" :: String }
         , "Kitchen" :: { "Remarks" :: String }
         )
    prefixed (card $ bodyLarge (text summaryLine)) identity # debounced summarySettleTime
    ( RecordToVariant.do
      button @"Submit order" { icon: "save" }
      button @"Receipt" { icon: "file" } )
    VariantToVariant.do
      ( VariantToRecord.do
        indeterminateLinearProgress
        snackbar @"Order submitted" submittedLine
        snackbar @"Submission failed" rejectionLine ) # action
          @( "Order submitted" :: { "Short ID" :: String }
           , "Receipt printed" :: { "Short ID" :: String }
           , "Submission failed" :: { "Short ID" :: String, reason :: String }
           ) submitOrder # atCase @"Submit order"
      ( VariantToRecord.do
        indeterminateLinearProgress
        snackbar @"Receipt printed" receiptLine ) # action printReceipt # atCase @"Receipt"

distanceEstimate
  :: PUI Web
    { "Address" :: String, distance :: [ estimated :: { km :: Int, to :: String }, unknown :: {} ] }
    { "Address" :: String, distance :: [ estimated :: { km :: Int, to :: String }, unknown :: {} ] }
distanceEstimate = Semigroupoid.do
  filledTextField @"Address" {}
  ( Semigroupoid.do
    button @"Estimate distance" { icon: "near_me" }
    indeterminateLinearProgress # action
      @( "Distance estimated" :: { "Address" :: String, distance :: [ estimated :: { km :: Int, to :: String }, unknown :: {} ] }
       ) estimateDistance # atCase @"Estimate distance"
    blankStatus @"Distance estimated" # fold identity ) # updated setDistance # settled staleDistanceForgotten # suffixed (bodyLarge (text distanceLine) # pane @"estimated" @( estimated :: { km :: Int }, unknown :: {} ) distanceOf)
