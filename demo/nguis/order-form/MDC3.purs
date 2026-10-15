module OrderForm.MDC3 (orderForm) where

import Prelude ((#), ($), identity, Unit)

import Data.Profunctor.Row.RecordToVariant as RecordToVariant
import Data.Profunctor.Row.VariantToRecord as VariantToRecord
import Effect (Effect)
import OrderForm (distanceLine, estimateDistance, fulfillmentMode, loadOrder, orderLine, payingLine, printReceipt, setDistance, staleDistanceForgotten, submitOrder, summaryLine)
import PUI (action, fold, joined, kept, load, looped, settled)
import PUI.Web ((<+>), choice, inCaseAt, provided, text)
import PUI.Web.MDC3 (body, bodyLarge, button', card, filledTextArea, filledTextField, group, headlineSmall, indeterminateLinearProgress, segmentedButton, snackbar, tabBar)
import QualifiedDo.Semigroupoid as Semigroupoid

orderForm :: Effect Unit
orderForm =
  body $ Semigroupoid.do
    indeterminateLinearProgress # load @( "Order loaded" :: _ ) loadOrder
    snackbar @"Order loaded" identity
    ( Semigroupoid.do
      ( Semigroupoid.do
        headlineSmall $ text orderLine
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
            ( bodyLarge $ text distanceLine ) # inCaseAt @"estimated" @"distance" ) # inCaseAt @"Delivery" @"Mode"
        group @"Payment" $ Semigroupoid.do
          filledTextField @"Total"
          segmentedButton @"Method"
            (choice @"cash" <+> choice @"card")
          filledTextField @"Paid"
          bodyLarge $ text payingLine
        group @"Kitchen" $ filledTextArea @"Remarks" { columns: 80, rows: 3 } ) # looped
      card $ bodyLarge $ text summaryLine
      RecordToVariant.do
        ( Semigroupoid.do
          button' @"Estimate distance" { icon: "near_me" } # provided @"Delivery" @( "Dine in" :: {}, "Takeaway" :: {}, "Delivery" :: {} ) fulfillmentMode
          indeterminateLinearProgress # action @"Estimate distance" @( "Distance estimated" :: { km :: Int, to :: String } ) estimateDistance ) # joined @"Distance estimated"
        ( Semigroupoid.do
          button' @"Submit order" { icon: "save" }
          indeterminateLinearProgress # action @"Submit order"
            @( "Order submitted" :: {}, "Order rejected: no total" :: {} ) submitOrder ) # joined @"Order submitted" # joined @"Order rejected: no total"
        ( Semigroupoid.do
          button' @"Receipt" { icon: "file" }
          indeterminateLinearProgress # action @"Receipt" @( "Receipt printed" :: {} ) printReceipt ) # joined @"Receipt printed"
      VariantToRecord.do
        fold @"Distance estimated" setDistance
        snackbar @"Order submitted" kept
        snackbar @"Order rejected: no total" kept
        snackbar @"Receipt printed" kept ) # looped
      @( "Identifier" :: { "Short ID" :: String, "Unique ID" :: String }
       , "Customer" :: { "First name" :: String, "Last name" :: String }
       , "Fulfillment" :: { "Mode" :: [ "Dine in" :: {}, "Takeaway" :: {}, "Delivery" :: {} ], "Table" :: String, "Time" :: String, "Address" :: String, distance :: [ estimated :: { km :: Int, to :: String }, unknown :: {} ] }
       , "Payment" :: { "Total" :: String, "Method" :: [ "cash" :: {}, "card" :: {} ], "Paid" :: String }
       , "Kitchen" :: { "Remarks" :: String }
       )
