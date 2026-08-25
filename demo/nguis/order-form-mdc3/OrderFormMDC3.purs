module OrderFormMDC3 (orderFormMDC3) where

import Prelude (identity, Unit, (#), ($))

import Data.Profunctor.Row.RecordToRecord as RecordToRecord
import Data.Profunctor.Row.RecordToVariant as RecordToVariant
import Data.Profunctor.Row.VariantToRecord as VariantToRecord
import Data.Profunctor.Row.VariantToVariant as VariantToVariant
import Data.Variant.Case (caseText)
import Effect (Effect)
import OrderFormLogic (deliveryDetail, dineInDetail, distanceKm, fulfillmentCase, fulfillmentState, loadOrder, printReceipt, receiptLine, rejectionLine, selection, submitOrder, submittedLine, summarySettleTime, takeawayDetail)
import PUI (action, armed, atCase, atField, bracketed, debounced, field, forCase, projection, looped, required, with)
import PUI.Web (choice)
import PUI.Web.HTML (inCase, shownWhen, shownAs, body, staticText, text)
import PUI.Web.MDC3 (bodyLarge, button, card, elevation5, filledTextArea, filledTextField, headlineSmall, indeterminateLinearProgress, segmentedButton, snackbar, tabBar, titleMedium)
import QualifiedDo.Semigroupoid as Semigroupoid

orderFormMDC3 :: Effect Unit
orderFormMDC3 =
  body $ ( elevation5 Semigroupoid.do
      indeterminateLinearProgress @"busy" # action loadOrder
      ( Semigroupoid.do
          ( headlineSmall $ RecordToRecord.do
              staticText "Order "
              text @"Short ID" ) # shownAs identity
          card $ Semigroupoid.do
            (titleMedium $ staticText "Identifier") # shownAs identity
            filledTextField @"Short ID" {}
            filledTextField @"Unique ID" {}
          card ( Semigroupoid.do
              (titleMedium $ staticText "Customer") # shownAs identity
              ( Semigroupoid.do
                  filledTextField @"First name" {}
                  filledTextField @"Last name" {}) # field @"customer" )
          card ( Semigroupoid.do
              (titleMedium $ staticText "Fulfillment") # shownAs identity
              ( ( Semigroupoid.do
                    tabBar @"selected"
                      [ choice @"Dine in", choice @"Takeaway", choice @"Delivery" ]
                    filledTextField @"Table" {} # inCase @"Dine in" selection
                    filledTextField @"Time" {} # inCase @"Takeaway" selection
                    ( Semigroupoid.do
                        filledTextField @"Address" {}
                        ( bodyLarge $ RecordToRecord.do
                            staticText "Distance "
                            text @"Address" # projection distanceKm
                            staticText " km" ) # shownAs identity) # inCase @"Delivery" selection) # bracketed fulfillmentState fulfillmentCase) # field @"fulfillment" )
          card $ Semigroupoid.do
            (titleMedium $ staticText "Total") # shownAs identity
            filledTextField @"Total" {}
          card ( Semigroupoid.do
              (titleMedium $ staticText "Payment") # shownAs identity
              ( Semigroupoid.do
                  segmentedButton @"Method"
                    [ choice @"cash", choice @"card" ] # required
                  filledTextField @"Paid" {}
                  ( bodyLarge $ RecordToRecord.do
                      staticText "Paying by "
                      text @"Method" # projection caseText ) # shownAs identity) # field @"payment" )
          card $ Semigroupoid.do
            (titleMedium $ staticText "Remarks") # shownAs identity
            filledTextArea @"Remarks" { columns: 80, rows: 3 }
      ) # looped
      bodyLarge ( Semigroupoid.do
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
              staticText ", fulfilled as "  ) # shownAs identity # debounced summarySettleTime
          ( RecordToRecord.do
              staticText "dine in at table "
              text @"Table" ) # shownWhen dineInDetail
          ( RecordToRecord.do
              staticText "takeaway at "
              text @"Time" ) # shownWhen takeawayDetail
          ( RecordToRecord.do
              staticText "delivery to "
              text @"Address"
              staticText " ("
              text @"Address" # projection distanceKm
              staticText " km away)" ) # shownWhen deliveryDetail
          ( ( RecordToRecord.do
              staticText ", paid "
              text @"Paid"
              staticText " by "
              text @"Method" # projection caseText ) # atField @"payment" ) # shownAs identity # debounced summarySettleTime )
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
