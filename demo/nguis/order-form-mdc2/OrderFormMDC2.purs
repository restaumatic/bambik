module OrderFormMDC2 (orderFormMDC2) where

import Prelude (Unit, (#), ($), show)

import Data.Profunctor.Row.RecordToRecord as RecordToRecord
import Data.Profunctor.Row.RecordToVariant as RecordToVariant
import Data.Profunctor.Row.VariantToRecord as VariantToRecord
import Data.Profunctor.Row.VariantToVariant as VariantToVariant
import Data.Variant (match)
import Data.Variant.Case (caseText)
import Effect (Effect)
import OrderFormLogic (deliveryDetail, deliveryDistance, dineInDetail, estimateDistance, fulfillmentCase, fulfillmentState, knownDistance, loadOrder, printReceipt, receiptLine, rejectionLine, selection, setDistance, staleDistanceForgotten, submitOrder, submittedLine, summarySettleTime, takeawayDetail)
import PUI (action, armed, atCase, atField, bracketed, debounced, field, forCase, projection, looped, required, settled, updated, with)
import PUI.Web (choice)
import PUI.Web.HTML (inCase, shownWhen, shown, body, staticText, text)
import PUI.Web.MDC2 (body1, button, card, elevation20, filledTextArea, filledTextField, headline6, indeterminateLinearProgress, segmentedButton, snackbar, subtitle1, tabBar)
import QualifiedDo.Semigroupoid as Pipeline

orderFormMDC2 :: Effect Unit
orderFormMDC2 =
  body $ ( elevation20 Pipeline.do
      indeterminateLinearProgress @"busy" # action loadOrder
      ( Pipeline.do
          ( headline6 $ RecordToRecord.do
              staticText "Order "
              text @"Short ID" ) # shown
          card $ Pipeline.do
            (subtitle1 $ staticText "Identifier") # shown
            filledTextField @"Short ID" {}
            filledTextField @"Unique ID" {}
          card ( Pipeline.do
              (subtitle1 $ staticText "Customer") # shown
              ( Pipeline.do
                  filledTextField @"First name" {}
                  filledTextField @"Last name" {}) # field @"customer" )
          card ( Pipeline.do
              (subtitle1 $ staticText "Fulfillment") # shown
              ( ( Pipeline.do
                    tabBar @"selected"
                      [ choice @"Dine in", choice @"Takeaway", choice @"Delivery" ]
                    filledTextField @"Table" {} # inCase @"Dine in" selection
                    filledTextField @"Time" {} # inCase @"Takeaway" selection
                    ( Pipeline.do
                        filledTextField @"Address" {} # settled staleDistanceForgotten
                        ( Pipeline.do
                            button @"Estimate distance" { icon: "near_me" }
                            indeterminateLinearProgress @"busy" # action estimateDistance # atCase @"Estimate distance" ) # updated (match { estimated: setDistance })
                        ( body1 $ RecordToRecord.do
                            staticText "Distance "
                            text @"km" # projection show
                            staticText " km" ) # shownWhen knownDistance) # inCase @"Delivery" selection) # bracketed fulfillmentState fulfillmentCase) # field @"fulfillment" )
          card $ Pipeline.do
            (subtitle1 $ staticText "Total") # shown
            filledTextField @"Total" {}
          card ( Pipeline.do
              (subtitle1 $ staticText "Payment") # shown
              ( Pipeline.do
                  segmentedButton @"Method"
                    [ choice @"cash", choice @"card" ] # required
                  filledTextField @"Paid" {}
                  ( body1 $ RecordToRecord.do
                      staticText "Paying by "
                      text @"Method" # projection caseText ) # shown) # field @"payment" )
          card $ Pipeline.do
            (subtitle1 $ staticText "Remarks") # shown
            filledTextArea @"Remarks" { columns: 80, rows: 3 }
      ) # looped
      body1 ( Pipeline.do
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
              text @"Table" ) # shownWhen dineInDetail
          ( RecordToRecord.do
              staticText "takeaway at "
              text @"Time" ) # shownWhen takeawayDetail
          ( RecordToRecord.do
              staticText "delivery to "
              text @"Address" ) # shownWhen deliveryDetail
          ( RecordToRecord.do
              staticText " ("
              text @"km" # projection show
              staticText " km away)" ) # shownWhen deliveryDistance
          ( ( RecordToRecord.do
              staticText ", paid "
              text @"Paid"
              staticText " by "
              text @"Method" # projection caseText ) # atField @"payment" ) # shown # debounced summarySettleTime )
      ( RecordToVariant.do
          button @"Submit order" { icon: "save" }
          button @"Receipt" { icon: "file" }) # armed
      VariantToVariant.do
        indeterminateLinearProgress @"busy" # action submitOrder # atCase @"Submit order"
        indeterminateLinearProgress @"busy" # action printReceipt # atCase @"Receipt"
      VariantToRecord.do
        snackbar # forCase @"orderSubmitted" submittedLine
        snackbar # forCase @"submissionFailed" rejectionLine
        snackbar # forCase @"receiptPrinted" receiptLine
  ) # with {}
