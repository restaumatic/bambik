module OrderFormMDC2 (orderFormMDC2) where

import Prelude (Unit, (#), ($))

import Data.Profunctor.Row.RecordToRecord as RecordToRecord
import Data.Profunctor.Row.RecordToVariant as RecordToVariant
import Data.Profunctor.Row.VariantToRecord as VariantToRecord
import Data.Profunctor.Row.VariantToVariant as VariantToVariant
import Data.Variant.Case (caseText)
import Effect (Effect)
import OrderFormLogic (deliveryDetail, deliveryPane, dineInDetail, dineInPane, distanceKm, fulfillmentCase, fulfillmentState, loadOrder, printReceipt, receiptLine, rejectionLine, setAddress, setTable, setTime, submitOrder, submittedLine, summarySettleTime, takeawayDetail, takeawayPane)
import PUI (action, armed, atCase, atField, bracketed, completed, debounced, tapped, field, forCase, projection, informed, required, updated, with)
import PUI.Web (choice)
import PUI.Web.HTML (body, provided, staticText, text)
import PUI.Web.MDC2 (body1, button, card, elevation20, filledTextArea, filledTextField, headline6, indeterminateLinearProgress, segmentedButton, snackbar, subtitle1, tabBar)
import QualifiedDo.Semigroupoid as Semigroupoid

orderFormMDC2 :: Effect Unit
orderFormMDC2 =
  body $ ( elevation20 Semigroupoid.do
      indeterminateLinearProgress @"busy" # action loadOrder
      RecordToRecord.do
        headline6 ( RecordToRecord.do
            staticText "Order "
            text @"Short ID" )
        card $ RecordToRecord.do
          subtitle1 $ staticText "Identifier"
          filledTextField @"Short ID" {}
          filledTextField @"Unique ID" {}
        card ( RecordToRecord.do
            subtitle1 $ staticText "Customer"
            ( RecordToRecord.do
                filledTextField @"First name" {}
                filledTextField @"Last name" {}) # field @"customer" )
        card ( RecordToRecord.do
            subtitle1 $ staticText "Fulfillment"
            ( ( Semigroupoid.do
                  tabBar @"selected"
                    [ choice @"Dine in", choice @"Takeaway", choice @"Delivery" ] # completed
                  filledTextField @"Table" {} # provided dineInPane # updated (informed setTable)
                  filledTextField @"Time" {} # provided takeawayPane # updated (informed setTime)
                  ( RecordToRecord.do
                      filledTextField @"Address" {}
                      body1 ( RecordToRecord.do
                          staticText "Distance "
                          text @"Address" # projection distanceKm
                          staticText " km" )) # provided deliveryPane # updated (informed setAddress)) # bracketed fulfillmentState fulfillmentCase) # field @"fulfillment" )
        card $ RecordToRecord.do
          subtitle1 $ staticText "Total"
          filledTextField @"Total" {}
        card ( RecordToRecord.do
            subtitle1 $ staticText "Payment"
            ( RecordToRecord.do
                segmentedButton @"Method"
                  [ choice @"cash", choice @"card" ] # required
                filledTextField @"Paid" {}
                body1 ( RecordToRecord.do
                    staticText "Paying by "
                    text @"Method" # projection caseText )) # field @"payment" )
        card $ RecordToRecord.do
          subtitle1 $ staticText "Remarks"
          filledTextArea @"Remarks" { columns: 80, rows: 3 }
      body1 ( Semigroupoid.do
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
              staticText ", fulfilled as " ) # debounced summarySettleTime # tapped
          ( RecordToRecord.do
              staticText "dine in at table "
              text @"Table" ) # provided dineInDetail # tapped
          ( RecordToRecord.do
              staticText "takeaway at "
              text @"Time" ) # provided takeawayDetail # tapped
          ( RecordToRecord.do
              staticText "delivery to "
              text @"Address"
              staticText " ("
              text @"Address" # projection distanceKm
              staticText " km away)" ) # provided deliveryDetail # tapped
          ( RecordToRecord.do
              staticText ", paid "
              text @"Paid"
              staticText " by "
              text @"Method" # projection caseText ) # atField @"payment" # debounced summarySettleTime # tapped )
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
