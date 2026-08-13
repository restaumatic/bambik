module OrderFormMDC3 (orderFormMDC3) where

import Prelude (identity, (#), ($), Unit)

import Data.Profunctor.Row.RecordToRecord as RecordToRecord
import Data.Profunctor.Row.RecordToVariant as RecordToVariant
import Data.Profunctor.Row.VariantToRecord as VariantToRecord
import Data.Profunctor.Row.VariantToVariant as VariantToVariant
import Effect (Effect)
import OrderFormLogic (deliveryDetail, deliveryPane, dineInDetail, dineInPane, distanceKm, fulfillmentCase, fulfillmentState, loadOrder, methodText, printReceipt, receiptLine, rejectionLine, setAddress, setTable, setTime, submitOrder, submittedLine, summarySettleTime, takeawayDetail, takeawayPane)
import PUI (action, asCase, asField, atCase, atField, bracketed, completed, debounced, displayed, field, forCase, forField, informed, required, tapped, updated, with)
import Data.Profunctor.Row (widenRecordInput)
import PUI.Web.HTML (body, provided, staticText, text)
import PUI.Web.MDC3 (bodyLarge, button, card, elevation5, filledTextArea, filledTextField, headlineSmall, indeterminateLinearProgress, segmentedButton, snackbar, tabBar)
import QualifiedDo.Semigroupoid as Semigroupoid

orderFormMDC3 :: Effect Unit
orderFormMDC3 =
  body $ ( elevation5 Semigroupoid.do
      indeterminateLinearProgress # action loadOrder
      RecordToRecord.do
        headlineSmall ( RecordToRecord.do
            staticText "Order "
            text # forField @"value" @"shortId" identity )
        card { caption: "Identifier" } $ RecordToRecord.do
          filledTextField { floatingLabel: "Short ID" } # asField @"value" @"shortId"
          filledTextField { floatingLabel: "Unique ID" } # asField @"value" @"orderId"
        card { caption: "Customer" }
          ( RecordToRecord.do
              filledTextField { floatingLabel: "First name" } # asField @"value" @"firstName"
              filledTextField { floatingLabel: "Last name" } # asField @"value" @"lastName") # field @"customer"
        card { caption: "Fulfillment" }
          ( ( Semigroupoid.do
                tabBar
                  [ { value: .dineIn {}, label: "Dine in" }
                  , { value: .takeaway {}, label: "Takeaway" }
                  , { value: .delivery {}, label: "Delivery" }
                  ] # asField @"value" @"selected" # completed
                filledTextField { floatingLabel: "Table" } # asField @"value" @"table" # provided dineInPane # updated (informed setTable)
                filledTextField { floatingLabel: "Time" } # asField @"value" @"time" # provided takeawayPane # updated (informed setTime)
                ( RecordToRecord.do
                    filledTextField { floatingLabel: "Address" } # asField @"value" @"address"
                    bodyLarge ( RecordToRecord.do
                        staticText "Distance "
                        text # forField @"value" @"address" distanceKm
                        staticText " km" )) # provided deliveryPane # updated (informed setAddress)) # bracketed fulfillmentState fulfillmentCase) # field @"fulfillment"
        card { caption: "Total" } $ filledTextField { floatingLabel: "Total" } # asField @"value" @"total"
        card { caption: "Payment" }
          ( RecordToRecord.do
              segmentedButton
                [ { value: .cash {}, label: "Cash" }
                , { value: .card {}, label: "Card" }
                ] # required @"value" # asField @"value" @"method"
              filledTextField { floatingLabel: "Paid" } # asField @"value" @"paid"
              bodyLarge ( RecordToRecord.do
                  staticText "Paying by "
                  text # forField @"value" @"method" methodText )) # field @"payment"
        card { caption: "Remarks" } $ filledTextArea { columns: 80, rows: 3 } # asField @"value" @"remarks"
      bodyLarge ( Semigroupoid.do
          ( RecordToRecord.do
              staticText "Summary: Order "
              text # forField @"value" @"shortId" identity
              staticText " (uniquely "
              text # forField @"value" @"orderId" identity
              staticText ") for "
              ( RecordToRecord.do
                  text # forField @"value" @"firstName" identity
                  staticText " "
                  text # forField @"value" @"lastName" identity ) # atField @"customer"
              staticText ", fulfilled as " ) # debounced summarySettleTime # tapped
          ( RecordToRecord.do
              staticText "dine in at table "
              text # forField @"value" @"table" identity ) # provided dineInDetail # displayed
          ( RecordToRecord.do
              staticText "takeaway at "
              text # forField @"value" @"time" identity ) # provided takeawayDetail # displayed
          ( RecordToRecord.do
              staticText "delivery to "
              text # forField @"value" @"address" identity
              staticText " ("
              text # forField @"value" @"address" distanceKm
              staticText " km away)" ) # provided deliveryDetail # displayed
          ( RecordToRecord.do
              staticText ", paid "
              text # forField @"value" @"paid" identity
              staticText " by "
              text # forField @"value" @"method" methodText ) # field @"payment" # debounced summarySettleTime # tapped )
      ( RecordToVariant.do
          button { label: "Submit order", icon: "save" } # asCase @"clicked" @"submit"
          button { label: "Receipt", icon: "file" } # asCase @"clicked" @"printReceipt") # widenRecordInput
      VariantToVariant.do
        indeterminateLinearProgress # action submitOrder # atCase @"submit"
        indeterminateLinearProgress # action printReceipt # atCase @"printReceipt"
      VariantToRecord.do
        snackbar # forCase @"event" @"orderSubmitted" submittedLine
        snackbar # forCase @"event" @"submissionFailed" rejectionLine
        snackbar # forCase @"event" @"receiptPrinted" receiptLine
  ) # with {}
