module PaymentMDC3 (paymentMDC3) where

import Prelude ((#), ($), (<<<), Unit, const)

import Data.Profunctor.Row.RecordToRecord as RecordToRecord
import Data.Profunctor.Row.VariantToVariant (iterate)
import Data.Variant (match)
import Effect (Effect)
import PaymentLogic (chargeFlaky, presentPayment, recordCharged, retryLine, startCharge, unpaidOrder)
import PUI (action, atCase, forCase, mvu, observed, settled, toCases, updated)
import PUI.Web.HTML (shown, body, staticText, text)
import PUI.Web.MDC3 (bodyMedium, button, card, elevation5, headlineSmall, indeterminateCircularProgress, snackbar)
import QualifiedDo.Category as Category

paymentMDC3 :: Effect Unit
paymentMDC3 =
  body $
    elevation5 $
      card $ ( Category.do
          ( headlineSmall $ RecordToRecord.do
              staticText "Amount due: $"
              text @"amountText" ) # shown
          (bodyMedium (text @"statusText")) # shown
          ( Category.do
              button @"Charge card" { icon: "credit_card" } # toCases startCharge
              ( Category.do
                  indeterminateCircularProgress @"busy" # action chargeFlaky # atCase @"charge"
                  snackbar # forCase @"charge" retryLine # observed ) # iterate ) # updated (match { charged: const <<< recordCharged })
      ) # settled presentPayment # mvu unpaidOrder
