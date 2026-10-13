module OrderFormMDC2 (orderFormMDC2) where

import Prelude (identity, Unit, (#), ($))

import Data.Profunctor.Row.RecordToVariant as RecordToVariant
import Data.Profunctor.Row.VariantToRecord as VariantToRecord
import Effect (Effect)
import OrderFormViewModel (deliveryAddress, distanceLine, distanceOf, estimateDistance, loadOrder, orderLine, orderLoadedLine, payingLine, printReceipt, receiptLine, rejectionLine, setDistance, staleDistanceForgotten, submitOrder, submittedLine, summaryLine)
import PUI (action, atCase, fold, joined, looped, prefixed, settled, suffixed, unshown)
import PUI.Web ((<+>), choice, inCaseAt, pane, provided, text)
import PUI.Web.MDC2 (body, body1, buttonWith, card, filledTextArea, filledTextField, group, headline6, indeterminateLinearProgress, segmentedButton, snackbar, tabBar)
import QualifiedDo.Semigroupoid as Semigroupoid

orderFormMDC2 :: Effect Unit
orderFormMDC2 =
  body $ Semigroupoid.do
    indeterminateLinearProgress # action loadOrder
    snackbar @"Order loaded" orderLoadedLine # fold identity
    ( Semigroupoid.do
      ( Semigroupoid.do
        prefixed (headline6 $ text orderLine) $ group @"Identifier" $ Semigroupoid.do
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
          filledTextField @"Address" # settled staleDistanceForgotten # suffixed (body1 (text distanceLine) # pane @"estimated" @( estimated :: { km :: Int }, unknown :: {} ) distanceOf) # inCaseAt @"Delivery" @"Mode"
        group @"Payment" $ Semigroupoid.do
          filledTextField @"Total"
          segmentedButton @"Method"
            (choice @"cash" <+> choice @"card")
          filledTextField @"Paid" # suffixed (body1 $ text payingLine)
        group @"Kitchen" $ filledTextArea @"Remarks" { columns: 80, rows: 3 } ) # looped
      prefixed (card $ body1 (text summaryLine)) identity
      RecordToVariant.do
        ( Semigroupoid.do
          buttonWith @"Estimate distance" { icon: "near_me" } # provided @"Delivery" @( "Dine in" :: {}, "Takeaway" :: {}, "Delivery" :: { "Address" :: String } ) deliveryAddress
          indeterminateLinearProgress # action @( "Distance estimated" :: { km :: Int, to :: String } ) estimateDistance # atCase @"Estimate distance" ) # joined @"Distance estimated"
        buttonWith @"Submit order" { icon: "save" }
        buttonWith @"Receipt" { icon: "file" }
      VariantToRecord.do
        unshown @"Distance estimated" # fold setDistance
        ( Semigroupoid.do
          indeterminateLinearProgress # action
            @( "Order submitted" :: { "Short ID" :: String }
             , "Submission failed" :: { "Short ID" :: String, reason :: String }
             ) submitOrder
          VariantToRecord.do
            snackbar @"Order submitted" submittedLine
            snackbar @"Submission failed" rejectionLine ) # atCase @"Submit order"
        ( Semigroupoid.do
          indeterminateLinearProgress # action @( "Receipt printed" :: { "Short ID" :: String } ) printReceipt
          snackbar @"Receipt printed" receiptLine ) # atCase @"Receipt" ) # looped
      @( "Identifier" :: { "Short ID" :: String, "Unique ID" :: String }
       , "Customer" :: { "First name" :: String, "Last name" :: String }
       , "Fulfillment" :: { "Mode" :: [ "Dine in" :: {}, "Takeaway" :: {}, "Delivery" :: {} ], "Table" :: String, "Time" :: String, "Address" :: String, distance :: [ estimated :: { km :: Int, to :: String }, unknown :: {} ] }
       , "Payment" :: { "Total" :: String, "Method" :: [ "cash" :: {}, "card" :: {} ], "Paid" :: String }
       , "Kitchen" :: { "Remarks" :: String }
       )
