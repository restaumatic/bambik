module PaymentMDC3 (paymentMDC3) where

import Prelude ((#), ($), Unit)

import Data.Profunctor.Row.VariantToVariant (iterate)
import Data.Variant (match)
import Effect (Effect)
import PaymentLogic (amountLine, chargeFlaky, recordCharged, retryLine, startCharge, statusLine, unpaidOrder)
import PUI (action, atCase, mvu, observed, toCase, updated)
import PUI.Web (shown, text)
import PUI.Web.MDC3 (body, bodyMedium, button, headlineSmall, indeterminateCircularProgress, snackbar)
import QualifiedDo.Semigroupoid as Semigroupoid

paymentMDC3 :: Effect Unit
paymentMDC3 =
  body $
    ( Semigroupoid.do
      ( headlineSmall $ text amountLine ) # shown
      ( bodyMedium $ text statusLine ) # shown
      ( Semigroupoid.do
        button @"Charge card" { icon: "credit_card" } # toCase @"charge" startCharge
        ( Semigroupoid.do
          indeterminateCircularProgress @"Charging card" # action chargeFlaky # atCase @"charge"
          snackbar @"charge" retryLine # observed ) # iterate ) # updated (match { charged: recordCharged })
    ) # mvu unpaidOrder
