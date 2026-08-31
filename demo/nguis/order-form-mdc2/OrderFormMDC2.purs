module OrderFormMDC2 (orderFormMDC2) where

import Prelude (Unit, (#), ($))

import Data.Profunctor.Row.RecordToRecord as RecordToRecord
import Data.Profunctor.Row.RecordToVariant as RecordToVariant
import Data.Profunctor.Row.VariantToRecord as VariantToRecord
import Data.Profunctor.Row.VariantToVariant as VariantToVariant
import Data.Variant (match)
import Effect (Effect)
import OrderFormLogic (deliveryDistance, estimateDistance, fulfillmentCase, distanceOf, fulfillmentOf, fulfillmentState, loadOrder, presentOrder, printReceipt, receiptLine, rejectionLine, selection, setDistance, staleDistanceForgotten, submitOrder, submittedLine, summarySettleTime)
import PUI (action, armed, atCase, atField, bracketed, debounced, field, forCases, forProperty, looped, required, settled, updated, with)
import PUI.Web (choice)
import PUI.Web.HTML (inCase, shownWhen, shown, body, staticText, text)
import PUI.Web.MDC2 (body1, button, card, elevation20, filledTextArea, filledTextField, headline6, indeterminateLinearProgress, segmentedButton, snackbar, subtitle1, tabBar)
import QualifiedDo.Category as Category

orderFormMDC2 :: Effect Unit
orderFormMDC2 =
  body $ ( elevation20 Category.do
      indeterminateLinearProgress @"busy" # action loadOrder
      ( Category.do
          ( headline6 $ RecordToRecord.do
              staticText "Order "
              text @"Short ID" ) # shown
          card $ Category.do
            (subtitle1 $ staticText "Identifier") # shown
            filledTextField @"Short ID" {}
            filledTextField @"Unique ID" {}
          card ( Category.do
              (subtitle1 $ staticText "Customer") # shown
              ( Category.do
                  filledTextField @"First name" {}
                  filledTextField @"Last name" {} ) # field @"customer" )
          card ( Category.do
              (subtitle1 $ staticText "Fulfillment") # shown
              ( ( Category.do
                    tabBar @"selected"
                      [ choice @"Dine in", choice @"Takeaway", choice @"Delivery" ]
                    filledTextField @"Table" {} # inCase @"Dine in" selection
                    filledTextField @"Time" {} # inCase @"Takeaway" selection
                    ( Category.do
                        filledTextField @"Address" {} # settled staleDistanceForgotten
                        ( Category.do
                            button @"Estimate distance" { icon: "near_me" }
                            indeterminateLinearProgress @"busy" # action estimateDistance # atCase @"Estimate distance" ) # updated (match { estimated: setDistance })
                        ( body1 $ RecordToRecord.do
                            staticText "Distance "
                            text @"kmText"
                            staticText " km" ) # shownWhen @"estimated" distanceOf ) # inCase @"Delivery" selection ) # bracketed fulfillmentState fulfillmentCase ) # field @"fulfillment" )
          card $ Category.do
            (subtitle1 $ staticText "Total") # shown
            filledTextField @"Total" {}
          card ( Category.do
              (subtitle1 $ staticText "Payment") # shown
              ( Category.do
                  segmentedButton @"Method"
                    [ choice @"cash", choice @"card" ] # required
                  filledTextField @"Paid" {}
                  ( body1 $ RecordToRecord.do
                      staticText "Paying by "
                      text @"methodText" ) # shown ) # field @"payment" )
          card $ Category.do
            (subtitle1 $ staticText "Remarks") # shown
            filledTextArea @"Remarks" { columns: 80, rows: 3 }
      ) # settled presentOrder # looped
      body1 ( Category.do
          ( RecordToRecord.do
              staticText "Summary: Order "
              text @"Short ID"
              staticText " (uniquely "
              text @"Unique ID"
              staticText ") for "
              ( RecordToRecord.do
                  text @"First name"
                  staticText " "
                  text @"Last name" ) # atField @"customer"
              staticText ", fulfilled as "  ) # shown # debounced summarySettleTime
          ( RecordToRecord.do
              staticText "dine in at table "
              text @"Table" ) # shownWhen @"Dine in" fulfillmentOf
          ( RecordToRecord.do
              staticText "takeaway at "
              text @"Time" ) # shownWhen @"Takeaway" fulfillmentOf
          ( RecordToRecord.do
              staticText "delivery to "
              text @"Address" ) # forProperty # shownWhen @"Delivery" fulfillmentOf
          ( RecordToRecord.do
              staticText " ("
              text @"kmText"
              staticText " km away)" ) # shownWhen @"estimated" deliveryDistance
          text @"paidLine" # shown # debounced summarySettleTime )
      ( RecordToVariant.do
          button @"Submit order" { icon: "save" }
          button @"Receipt" { icon: "file" } ) # armed
      VariantToVariant.do
        indeterminateLinearProgress @"busy" # action submitOrder # atCase @"Submit order"
        indeterminateLinearProgress @"busy" # action printReceipt # atCase @"Receipt"
      VariantToRecord.do
        snackbar # forCases (match { orderSubmitted: submittedLine })
        snackbar # forCases (match { submissionFailed: rejectionLine })
        snackbar # forCases (match { receiptPrinted: receiptLine })
  ) # with {}
