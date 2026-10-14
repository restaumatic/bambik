module OrderForm.MDC2 (orderForm) where

import Prelude ((#), ($), identity, Unit)

import Data.Profunctor.Row.RecordToVariant as RecordToVariant
import Data.Profunctor.Row.VariantToRecord as VariantToRecord
import Effect (Effect)
import OrderForm (distanceLine, estimateDistance, fulfillmentMode, loadOrder, orderLine, orderLoadedLine, payingLine, printReceipt, receiptLine, rejectionLine, setDistance, staleDistanceForgotten, submitOrder, submittedLine, summaryLine)
import PUI (action, atCase, fold, joined, kept, looped, settled)
import PUI.Web ((<+>), choice, inCaseAt, provided, text)
import PUI.Web.MDC2 (body, body1, button', card, filledTextArea, filledTextField, group, headline6, indeterminateLinearProgress, segmentedButton, snackbar, tabBar)
import QualifiedDo.Semigroupoid as Semigroupoid

orderForm :: Effect Unit
orderForm =
  body $ Semigroupoid.do
    indeterminateLinearProgress # action loadOrder
    snackbar @"Order loaded" orderLoadedLine identity
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
          tabBar @"Mode"
            (choice @"Dine in" <+> choice @"Takeaway" <+> choice @"Delivery")
          filledTextField @"Table" # inCaseAt @"Dine in" @"Mode"
          filledTextField @"Time" # inCaseAt @"Takeaway" @"Mode"
          ( Semigroupoid.do
            filledTextField @"Address" # settled staleDistanceForgotten
            ( body1 $ text distanceLine ) # inCaseAt @"estimated" @"distance" ) # inCaseAt @"Delivery" @"Mode"
        group @"Payment" $ Semigroupoid.do
          filledTextField @"Total"
          segmentedButton @"Method"
            (choice @"cash" <+> choice @"card")
          filledTextField @"Paid"
          body1 $ text payingLine
        group @"Kitchen" $ filledTextArea @"Remarks" { columns: 80, rows: 3 } ) # looped
      card $ body1 $ text summaryLine
      RecordToVariant.do
        ( Semigroupoid.do
          button' @"Estimate distance" { icon: "near_me" } # provided @"Delivery" @( "Dine in" :: {}, "Takeaway" :: {}, "Delivery" :: {} ) fulfillmentMode
          indeterminateLinearProgress # action @( "Distance estimated" :: { km :: Int, to :: String } ) estimateDistance # atCase @"Estimate distance" ) # joined
        ( Semigroupoid.do
          button' @"Submit order" { icon: "save" }
          indeterminateLinearProgress # action
            @( "Order submitted" :: { "Short ID" :: String }
             , "Submission failed" :: { "Short ID" :: String, reason :: String }
             ) submitOrder # atCase @"Submit order" ) # joined
        ( Semigroupoid.do
          button' @"Receipt" { icon: "file" }
          indeterminateLinearProgress # action @( "Receipt printed" :: { "Short ID" :: String } ) printReceipt # atCase @"Receipt" ) # joined
      VariantToRecord.do
        fold @"Distance estimated" setDistance
        snackbar @"Order submitted" submittedLine kept
        snackbar @"Submission failed" rejectionLine kept
        snackbar @"Receipt printed" receiptLine kept ) # looped
      @( "Identifier" :: { "Short ID" :: String, "Unique ID" :: String }
       , "Customer" :: { "First name" :: String, "Last name" :: String }
       , "Fulfillment" :: { "Mode" :: [ "Dine in" :: {}, "Takeaway" :: {}, "Delivery" :: {} ], "Table" :: String, "Time" :: String, "Address" :: String, distance :: [ estimated :: { km :: Int, to :: String }, unknown :: {} ] }
       , "Payment" :: { "Total" :: String, "Method" :: [ "cash" :: {}, "card" :: {} ], "Paid" :: String }
       , "Kitchen" :: { "Remarks" :: String }
       )
