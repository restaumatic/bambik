module PaymentMDC2 (paymentMDC2) where

import Prelude ((#), ($), (<<<), Unit, const)

import Data.Profunctor.Row.RecordToRecord as RecordToRecord
import Data.Profunctor.Row.VariantToVariant (iterate)
import Data.Variant (match)
import Effect (Effect)
import PaymentLogic (chargeFlaky, presentPayment, recordCharged, retryLine, startCharge, unpaidOrder)
import PUI (action, forCases, mvu, observed, atCase, settled, toCases, updated)
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
              text @"amountText" ) # shown
          (body2 (text @"statusText")) # shown
          ( Category.do
              button @"Charge card" { icon: "credit_card" } # toCases startCharge
              ( Category.do
                  indeterminateCircularProgress @"busy" # action chargeFlaky # atCase @"charge"
                  snackbar # forCases (match { charge: retryLine }) # observed ) # iterate ) # updated (match { charged: const <<< recordCharged })
      ) # settled presentPayment # mvu unpaidOrder
