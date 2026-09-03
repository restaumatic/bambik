module OrderFormMDC2 (orderFormMDC2) where

import Prelude (Unit, (#), ($))

import Data.Profunctor.Row.RecordToVariant as RecordToVariant
import Data.Profunctor.Row.VariantToRecord as VariantToRecord
import Data.Profunctor.Row.VariantToVariant as VariantToVariant
import Data.Variant (match)
import Effect (Effect)
import OrderFormLogic (awayLine, deliveryDistance, deliveryLine, dineInLine, distanceLine, distanceOf, estimateDistance, fulfillmentCase, fulfillmentOf, fulfillmentState, loadOrder, orderLine, paidLine, payingLine, printReceipt, receiptLine, rejectionLine, selection, setDistance, staleDistanceForgotten, submitOrder, submittedLine, summaryLine, summarySettleTime, takeawayLine)
import PUI (action, armed, atCase, bracketed, debounced, field, forCase, looped, required, settled, updated, with)
import PUI.Web (choice)
import PUI.Web.HTML (inCase, shownWhen, shown, body, staticText, text)
import PUI.Web.MDC2 (body1, button, card, elevation20, filledTextArea, filledTextField, headline6, indeterminateLinearProgress, segmentedButton, snackbar, subtitle1, tabBar)
import QualifiedDo.Category as Category

orderFormMDC2 :: Effect Unit
orderFormMDC2 =
  body $ ( elevation20 Category.do
      indeterminateLinearProgress @"busy" # action loadOrder
      ( Category.do
          ( headline6 $ text orderLine ) # shown
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
                        ( body1 $ text distanceLine ) # shownWhen @"estimated" distanceOf ) # inCase @"Delivery" selection ) # bracketed fulfillmentState fulfillmentCase ) # field @"fulfillment" )
          card $ Category.do
            (subtitle1 $ staticText "Total") # shown
            filledTextField @"Total" {}
          card ( Category.do
              (subtitle1 $ staticText "Payment") # shown
              ( Category.do
                  segmentedButton @"Method"
                    [ choice @"cash", choice @"card" ] # required
                  filledTextField @"Paid" {}
                  ( body1 $ text payingLine ) # shown ) # field @"payment" )
          card $ Category.do
            (subtitle1 $ staticText "Remarks") # shown
            filledTextArea @"Remarks" { columns: 80, rows: 3 }
      ) # looped
      body1 ( Category.do
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
