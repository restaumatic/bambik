module OrderFormMDC3 (orderFormMDC3) where

import Prelude (Unit, (#), ($))

import Data.Profunctor.Row.RecordToVariant as RecordToVariant
import Data.Profunctor.Row.VariantToRecord as VariantToRecord
import Data.Profunctor.Row.VariantToVariant as VariantToVariant
import Data.Variant (match)
import Effect (Effect)
import OrderFormLogic (awayLine, deliveryDistance, deliveryLine, dineInLine, distanceLine, distanceOf, estimateDistance, fulfillmentCase, fulfillmentOf, fulfillmentState, loadOrder, orderLine, paidLine, payingLine, printReceipt, receiptLine, rejectionLine, selection, setDistance, staleDistanceForgotten, submitOrder, submittedLine, summaryLine, summarySettleTime, takeawayLine)
import PUI (action, armed, atCase, bracketed, debounced, forCase, looped, required, settled, updated, with)
import PUI.Web (choice)
import PUI.Web.HTML (inCase, shownWhen, shown, body, staticText, text)
import PUI.Web.MDC3 (bodyLarge, button, card, elevation5, filledTextArea, filledTextField, group, headlineSmall, indeterminateLinearProgress, segmentedButton, snackbar, tabBar, titleMedium)
import QualifiedDo.Category as Category

orderFormMDC3 :: Effect Unit
orderFormMDC3 =
  body $ ( elevation5 Category.do
      indeterminateLinearProgress @"busy" # action loadOrder
      ( Category.do
          ( headlineSmall $ text orderLine ) # shown
          card $ Category.do
            (titleMedium $ staticText "Identifier") # shown
            filledTextField @"Short ID" {}
            filledTextField @"Unique ID" {}
          ( Category.do
              filledTextField @"First name" {}
              filledTextField @"Last name" {} ) # group @"Customer"
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
                    ( bodyLarge $ text distanceLine ) # shownWhen @"estimated" distanceOf ) # inCase @"Delivery" selection ) # bracketed fulfillmentState fulfillmentCase ) # group @"Fulfillment"
          card $ Category.do
            (titleMedium $ staticText "Total") # shown
            filledTextField @"Total" {}
          ( Category.do
              segmentedButton @"Method"
                [ choice @"cash", choice @"card" ] # required
              filledTextField @"Paid" {}
              ( bodyLarge $ text payingLine ) # shown ) # group @"Payment"
          card $ Category.do
            (titleMedium $ staticText "Remarks") # shown
            filledTextArea @"Remarks" { columns: 80, rows: 3 }
      ) # looped
      bodyLarge ( Category.do
          text summaryLine # shown # debounced summarySettleTime
          text dineInLine # shownWhen @"Dine in" fulfillmentOf
          text takeawayLine # shownWhen @"Takeaway" fulfillmentOf
          text deliveryLine # shownWhen @"Delivery" fulfillmentOf
          text awayLine # shownWhen @"estimated" deliveryDistance
          text paidLine # shown # debounced summarySettleTime )
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
