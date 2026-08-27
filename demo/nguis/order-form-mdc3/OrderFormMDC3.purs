module OrderFormMDC3 (orderFormMDC3) where

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
import PUI.Web.MDC3 (bodyLarge, button, card, elevation5, filledTextArea, filledTextField, headlineSmall, indeterminateLinearProgress, segmentedButton, snackbar, tabBar, titleMedium)
import QualifiedDo.Category as Category

orderFormMDC3 :: Effect Unit
orderFormMDC3 =
  body $ ( elevation5 Category.do
      indeterminateLinearProgress @"busy" # action loadOrder
      ( Category.do
          ( headlineSmall $ RecordToRecord.do
              staticText "Order "
              text @"Short ID" ) # shown
          card $ Category.do
            (titleMedium $ staticText "Identifier") # shown
            filledTextField @"Short ID" {}
            filledTextField @"Unique ID" {}
          card ( Category.do
              (titleMedium $ staticText "Customer") # shown
              ( Category.do
                  filledTextField @"First name" {}
                  filledTextField @"Last name" {} ) # field @"customer" )
          card ( Category.do
              (titleMedium $ staticText "Fulfillment") # shown
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
                        ( bodyLarge $ RecordToRecord.do
                            staticText "Distance "
                            text @"km" # projection show
                            staticText " km" ) # shownWhen knownDistance ) # inCase @"Delivery" selection ) # bracketed fulfillmentState fulfillmentCase ) # field @"fulfillment" )
          card $ Category.do
            (titleMedium $ staticText "Total") # shown
            filledTextField @"Total" {}
          card ( Category.do
              (titleMedium $ staticText "Payment") # shown
              ( Category.do
                  segmentedButton @"Method"
                    [ choice @"cash", choice @"card" ] # required
                  filledTextField @"Paid" {}
                  ( bodyLarge $ RecordToRecord.do
                      staticText "Paying by "
                      text @"Method" # projection caseText ) # shown ) # field @"payment" )
          card $ Category.do
            (titleMedium $ staticText "Remarks") # shown
            filledTextArea @"Remarks" { columns: 80, rows: 3 }
      ) # looped
      bodyLarge ( Category.do
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
          button @"Receipt" { icon: "file" } ) # armed
      VariantToVariant.do
        indeterminateLinearProgress @"busy" # action submitOrder # atCase @"Submit order"
        indeterminateLinearProgress @"busy" # action printReceipt # atCase @"Receipt"
      VariantToRecord.do
        snackbar # forCase @"orderSubmitted" submittedLine
        snackbar # forCase @"submissionFailed" rejectionLine
        snackbar # forCase @"receiptPrinted" receiptLine
  ) # with {}
