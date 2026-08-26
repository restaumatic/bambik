module PaymentMDC2 (paymentMDC2) where

import Prelude ((#), ($), (<<<), Unit, const, show)

import Data.Profunctor.Row.RecordToRecord as RecordToRecord
import Data.Profunctor.Row.VariantToVariant (iterate)
import Data.Variant (match)
import Effect (Effect)
import PaymentLogic (chargeFlaky, recordCharged, retryLine, startCharge, statusLine, unpaidOrder)
import PUI (action, forCase, projection, mvu, observed, atCase, projected, toCases, updated)
import PUI.Web.HTML (shown, body, staticText, text)
import PUI.Web.MDC2 (body2, button, card, elevation20, headline6, indeterminateCircularProgress, snackbar)
import QualifiedDo.Category as Category

paymentMDC2 :: Effect Unit
paymentMDC2 =
  body $
    elevation20 $
      card $ ( Category.do
          ( headline6 $ RecordToRecord.do
              staticText "Amount due: $"
              text @"amount" # projection show ) # shown
          (body2 (text @"status") # projected statusLine) # shown
          ( Category.do
              button @"Charge card" { icon: "credit_card" } # toCases startCharge
              ( Category.do
                  indeterminateCircularProgress @"busy" # action chargeFlaky # atCase @"charge"
                  snackbar # forCase @"charge" retryLine # observed ) # iterate ) # updated (match { charged: const <<< recordCharged })
      ) # mvu unpaidOrder
