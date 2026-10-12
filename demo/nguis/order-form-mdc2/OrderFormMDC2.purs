module OrderFormMDC2 (orderFormMDC2) where

import Prelude (identity, Unit, (#), ($))

import Data.Profunctor.Row.RecordToVariant as RecordToVariant
import Data.Profunctor.Row.VariantToRecord as VariantToRecord
import Data.Profunctor.Row.VariantToVariant as VariantToVariant
import Effect (Effect)
import OrderFormViewModel (deliveryAddress, distanceLine, distanceOf, estimateDistance, loadOrder, orderLine, payingLine, printReceipt, receiptLine, rejectionLine, setDistance, staleDistanceForgotten, submitOrder, submittedLine, summaryLine)
import PUI (action, atCase, blankStatus, fold, joined, looped, prefixed, settled, subChoice, suffixed)
import PUI.Web ((<+>), choice, inCaseAt, pane, provided, text)
import PUI.Web.MDC2 (body, body1, button, card, filledTextArea, filledTextField, group, headline6, indeterminateLinearProgress, segmentedButton, snackbar, tabBar)
import QualifiedDo.Semigroupoid as Semigroupoid

orderFormMDC2 :: Effect Unit
orderFormMDC2 =
  body $ Semigroupoid.do
    indeterminateLinearProgress # action loadOrder
    blankStatus @"Order loaded" # fold identity
    ( Semigroupoid.do
      ( Semigroupoid.do
        prefixed (headline6 $ text orderLine) $ group @"Identifier" $ Semigroupoid.do
          filledTextField @"Short ID" {}
          filledTextField @"Unique ID" {}
        group @"Customer" $ Semigroupoid.do
          filledTextField @"First name" {}
          filledTextField @"Last name" {}
        group @"Fulfillment" $ Semigroupoid.do
          tabBar @"Mode"
            (choice @"Dine in" <+> choice @"Takeaway" <+> choice @"Delivery")
          filledTextField @"Table" {} # inCaseAt @"Dine in" @"Mode"
          filledTextField @"Time" {} # inCaseAt @"Takeaway" @"Mode"
          filledTextField @"Address" {} # settled staleDistanceForgotten # suffixed (body1 (text distanceLine) # pane @"estimated" @( estimated :: { km :: Int }, unknown :: {} ) distanceOf) # inCaseAt @"Delivery" @"Mode"
        group @"Payment" $ Semigroupoid.do
          filledTextField @"Total" {}
          segmentedButton @"Method"
            (choice @"cash" <+> choice @"card")
          filledTextField @"Paid" {} # suffixed (body1 $ text payingLine)
        group @"Kitchen" $ filledTextArea @"Remarks" { columns: 80, rows: 3 } ) # looped
      prefixed (card $ body1 (text summaryLine)) identity
      RecordToVariant.do
        ( Semigroupoid.do
          button @"Estimate distance" { icon: "near_me" } # provided @"Delivery" @( "Dine in" :: {}, "Takeaway" :: {}, "Delivery" :: { "Address" :: String } ) deliveryAddress
          indeterminateLinearProgress # action @( "Distance estimated" :: { km :: Int, to :: String } ) estimateDistance # atCase @"Estimate distance" ) # joined @"Distance estimated"
        button @"Submit order" { icon: "save" }
        button @"Receipt" { icon: "file" }
      ( VariantToVariant.do
        indeterminateLinearProgress # action
          @( "Order submitted" :: { "Short ID" :: String }
           , "Receipt printed" :: { "Short ID" :: String }
           , "Submission failed" :: { "Short ID" :: String, reason :: String }
           ) submitOrder # atCase @"Submit order"
        indeterminateLinearProgress # action printReceipt # atCase @"Receipt" ) # subChoice
      VariantToRecord.do
        blankStatus @"Distance estimated" # fold setDistance
        snackbar @"Order submitted" submittedLine
        snackbar @"Submission failed" rejectionLine
        snackbar @"Receipt printed" receiptLine ) # looped
      @( "Identifier" :: { "Short ID" :: String, "Unique ID" :: String }
       , "Customer" :: { "First name" :: String, "Last name" :: String }
       , "Fulfillment" :: { "Mode" :: [ "Dine in" :: {}, "Takeaway" :: {}, "Delivery" :: {} ], "Table" :: String, "Time" :: String, "Address" :: String, distance :: [ estimated :: { km :: Int, to :: String }, unknown :: {} ] }
       , "Payment" :: { "Total" :: String, "Method" :: [ "cash" :: {}, "card" :: {} ], "Paid" :: String }
       , "Kitchen" :: { "Remarks" :: String }
       )
