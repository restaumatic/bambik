module OrderFormMDC2 (orderFormMDC2) where

import Prelude (identity, (#), ($), Unit)

import Data.Profunctor.Row.RecordToRecord as RecordToRecord
import Data.Profunctor.Row.RecordToVariant as RecordToVariant
import Data.Profunctor.Row.VariantToRecord as VariantToRecord
import Data.Profunctor.Row.VariantToVariant as VariantToVariant
import Effect (Effect)
import OrderFormLogic (deliveryDetail, deliveryPane, dineInDetail, dineInPane, distanceKm, fulfillmentCase, fulfillmentState, loadOrder, methodText, printReceipt, receiptLine, rejectionLine, setAddress, setTable, setTime, submitOrder, submittedLine, summarySettleTime, takeawayDetail, takeawayPane)
import PUI (action, armed, asCase, atCase, atField, bracketed, completed, debounced, displayed, field, forCase, forField, informed, required, tapped, updated, with)
import PUI.Web.HTML (body, provided, staticText, text)
import PUI.Web.MDC2 (body1, button, card, elevation20, filledTextArea, filledTextField, headline6, indeterminateLinearProgress, segmentedButton, snackbar, tabBar)
import QualifiedDo.Semigroupoid as Semigroupoid

orderFormMDC2 :: Effect Unit
orderFormMDC2 =
  body $ ( elevation20 Semigroupoid.do
      indeterminateLinearProgress @"busy" # action loadOrder
      RecordToRecord.do
        headline6 ( RecordToRecord.do
            staticText "Order "
            text @"shortId" )
        card { caption: "Identifier" } $ RecordToRecord.do
          filledTextField @"shortId" { floatingLabel: "Short ID" }
          filledTextField @"orderId" { floatingLabel: "Unique ID" }
        card { caption: "Customer" }
          ( RecordToRecord.do
              filledTextField @"firstName" {}
              filledTextField @"lastName" {}) # field @"customer"
        card { caption: "Fulfillment" }
          ( ( Semigroupoid.do
                tabBar @"selected"
                  [ { value: .dineIn {}, label: "Dine in" }
                  , { value: .takeaway {}, label: "Takeaway" }
                  , { value: .delivery {}, label: "Delivery" }
                  ] # completed
                filledTextField @"table" {} # provided dineInPane # updated (informed setTable)
                filledTextField @"time" {} # provided takeawayPane # updated (informed setTime)
                ( RecordToRecord.do
                    filledTextField @"address" {}
                    body1 ( RecordToRecord.do
                        staticText "Distance "
                        text @"value" # forField @"address" distanceKm
                        staticText " km" )) # provided deliveryPane # updated (informed setAddress)) # bracketed fulfillmentState fulfillmentCase) # field @"fulfillment"
        card { caption: "Total" } $ filledTextField @"total" {}
        card { caption: "Payment" }
          ( RecordToRecord.do
              segmentedButton @"method"
                [ { value: .cash {}, label: "Cash" }
                , { value: .card {}, label: "Card" }
                ] # required
              filledTextField @"paid" {}
              body1 ( RecordToRecord.do
                  staticText "Paying by "
                  text @"value" # forField @"method" methodText )) # field @"payment"
        card { caption: "Remarks" } $ filledTextArea @"remarks" { columns: 80, rows: 3 }
      body1 ( Semigroupoid.do
          ( RecordToRecord.do
              staticText "Summary: Order "
              text @"shortId"
              staticText " (uniquely "
              text @"orderId"
              staticText ") for "
              ( RecordToRecord.do
                  text @"firstName"
                  staticText " "
                  text @"lastName" ) # atField @"customer"
              staticText ", fulfilled as " ) # debounced summarySettleTime # tapped
          ( RecordToRecord.do
              staticText "dine in at table "
              text @"table" ) # provided dineInDetail # displayed
          ( RecordToRecord.do
              staticText "takeaway at "
              text @"time" ) # provided takeawayDetail # displayed
          ( RecordToRecord.do
              staticText "delivery to "
              text @"address"
              staticText " ("
              text @"value" # forField @"address" distanceKm
              staticText " km away)" ) # provided deliveryDetail # displayed
          ( RecordToRecord.do
              staticText ", paid "
              text @"paid"
              staticText " by "
              text @"value" # forField @"method" methodText ) # field @"payment" # debounced summarySettleTime # tapped )
      ( RecordToVariant.do
          button { label: "Submit order", icon: "save" } # asCase @"clicked" @"submit"
          button { label: "Receipt", icon: "file" } # asCase @"clicked" @"printReceipt") # armed
      VariantToVariant.do
        indeterminateLinearProgress @"busy" # action submitOrder # atCase @"submit"
        indeterminateLinearProgress @"busy" # action printReceipt # atCase @"printReceipt"
      VariantToRecord.do
        snackbar # forCase @"event" @"orderSubmitted" submittedLine
        snackbar # forCase @"event" @"submissionFailed" rejectionLine
        snackbar # forCase @"event" @"receiptPrinted" receiptLine
  ) # with {}
