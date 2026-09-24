module PaymentMDC3 (paymentMDC3) where

import Prelude ((#), ($), (<<<), Unit, const)

import Data.Profunctor.Row.VariantToVariant (iterate)
import Data.Variant (match)
import Effect (Effect)
import PaymentLogic (amountLine, chargeFlaky, recordCharged, retryLine, startCharge, statusLine, unpaidOrder)
import PUI (action, atCase, forCase, mvu, observed, toCases, updated)
import PUI.Web (shown, text)
import PUI.Web.MDC3 (body, bodyMedium, button, card, headlineSmall, indeterminateCircularProgress, snackbar)
import QualifiedDo.Semigroupoid as Semigroupoid

paymentMDC3 :: Effect Unit
paymentMDC3 =
  body $
    card $ ( Semigroupoid.do
      ( headlineSmall $ text amountLine ) # shown
      ( bodyMedium $ text statusLine ) # shown
      ( Semigroupoid.do
        button @"Charge card" { icon: "credit_card" } # toCases startCharge
        ( Semigroupoid.do
          indeterminateCircularProgress @"busy" # action chargeFlaky # atCase @"charge"
          snackbar # forCase @"charge" retryLine # observed ) # iterate ) # updated (match { charged: const <<< recordCharged })
    ) # mvu unpaidOrder
