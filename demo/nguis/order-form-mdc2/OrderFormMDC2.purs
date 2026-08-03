module OrderFormMDC2 (orderFormMDC2) where

import Prelude (identity, (#), ($), Unit)

import Data.Profunctor.Row.RecordToRecord as RecordToRecord
import Data.Profunctor.Row.RecordToVariant as RecordToVariant
import Data.Profunctor.Row.VariantToRecord as VariantToRecord
import Data.Profunctor.Row.VariantToVariant as VariantToVariant
import Effect (Effect)
import OrderFormLogic (deliveryDetail, deliveryPane, dineInDetail, dineInPane, distanceKm, fulfillmentCase, fulfillmentState, loadOrder, methodText, printReceipt, receiptLine, rejectionLine, setAddress, setTable, setTime, submitOrder, submittedLine, summarySettleTime, takeawayDetail, takeawayPane)
import PUI (PUI, action, asCase, asField, atField, bracketed, completed, debounced, displayed, field, forCase, forField, informed, onCase, required, silence, tapped, updated, with)
import Data.Profunctor.Row (widenRecordInput)
import PUI.Web.HTML (body, provided, staticText, text)
import PUI.Web (Web)
import PUI.Web.MDC2 (body1, button, card, elevation20, filledTextArea, filledTextField, headline6, indeterminateLinearProgress, segmentedButton, snackbar, tabBar)
import QualifiedDo.Semigroupoid as Semigroupoid

orderFormMDC2 :: Effect Unit
orderFormMDC2 =
  body $ ( elevation20 Semigroupoid.do
      indeterminateLinearProgress # action loadOrder
      RecordToRecord.do
        headline6 ( RecordToRecord.do
            staticText "Order "
            text # forField @"shortId" identity )
        card { caption: "Identifier" } $ RecordToRecord.do
          filledTextField { floatingLabel: "Short ID" } # asField @"shortId"
          filledTextField { floatingLabel: "Unique ID" } # asField @"orderId"
        card { caption: "Customer" }
          ( RecordToRecord.do
              filledTextField { floatingLabel: "First name" } # asField @"firstName"
              filledTextField { floatingLabel: "Last name" } # asField @"lastName") # field @"customer"
        card { caption: "Fulfillment" }
          ( ( Semigroupoid.do
                tabBar
                  [ { value: .dineIn {}, label: "Dine in" }
                  , { value: .takeaway {}, label: "Takeaway" }
                  , { value: .delivery {}, label: "Delivery" }
                  ] # asField @"selected" # completed
                filledTextField { floatingLabel: "Table" } # asField @"table" # provided dineInPane # updated (informed setTable)
                filledTextField { floatingLabel: "Time" } # asField @"time" # provided takeawayPane # updated (informed setTime)
                ( RecordToRecord.do
                    filledTextField { floatingLabel: "Address" } # asField @"address"
                    body1 ( RecordToRecord.do
                        staticText "Distance "
                        text # forField @"address" distanceKm
                        staticText " km" )) # provided deliveryPane # updated (informed setAddress)) # bracketed fulfillmentState fulfillmentCase) # field @"fulfillment"
        card { caption: "Total" } $ filledTextField { floatingLabel: "Total" } # asField @"total"
        card { caption: "Payment" }
          ( RecordToRecord.do
              segmentedButton
                [ { value: .cash {}, label: "Cash" }
                , { value: .card {}, label: "Card" }
                ] # required # asField @"method"
              filledTextField { floatingLabel: "Paid" } # asField @"paid"
              body1 ( RecordToRecord.do
                  staticText "Paying by "
                  text # forField @"method" methodText )) # field @"payment"
        card { caption: "Remarks" } $ filledTextArea { columns: 80, rows: 3 } # asField @"remarks"
      body1 ( Semigroupoid.do
          ( RecordToRecord.do
              staticText "Summary: Order "
              text # forField @"shortId" identity
              staticText " (uniquely "
              text # forField @"orderId" identity
              staticText ") for "
              ( RecordToRecord.do
                  text # forField @"firstName" identity
                  staticText " "
                  text # forField @"lastName" identity ) # atField @"customer"
              staticText ", fulfilled as " ) # debounced summarySettleTime # tapped
          ( RecordToRecord.do
              staticText "dine in at table "
              text # forField @"table" identity ) # provided dineInDetail # displayed
          ( RecordToRecord.do
              staticText "takeaway at "
              text # forField @"time" identity ) # provided takeawayDetail # displayed
          ( RecordToRecord.do
              staticText "delivery to "
              text # forField @"address" identity
              staticText " ("
              text # forField @"address" distanceKm
              staticText " km away)" ) # provided deliveryDetail # displayed
          ( RecordToRecord.do
              staticText ", paid "
              text # forField @"paid" identity
              staticText " by "
              text # forField @"method" methodText ) # field @"payment" # debounced summarySettleTime # tapped )
      ( RecordToVariant.do
          button { label: "Submit order", icon: "save" } # asCase @"submit"
          button { label: "Receipt", icon: "file" } # asCase @"printReceipt") # widenRecordInput
      VariantToVariant.do
        indeterminateLinearProgress # action submitOrder # onCase @"submit"
        indeterminateLinearProgress # action printReceipt # onCase @"printReceipt"
      VariantToRecord.do
        submittedToast
        rejectionToast
        receiptToast
      silence
  ) # with {}

submittedToast :: PUI Web [ orderSubmitted :: { shortId :: String } ] {}
submittedToast = snackbar # forCase @"orderSubmitted" submittedLine

rejectionToast :: PUI Web [ submissionFailed :: { shortId :: String, reason :: String } ] {}
rejectionToast = snackbar # forCase @"submissionFailed" rejectionLine

receiptToast :: PUI Web [ receiptPrinted :: { shortId :: String } ] {}
receiptToast = snackbar # forCase @"receiptPrinted" receiptLine
