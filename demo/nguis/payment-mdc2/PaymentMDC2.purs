module PaymentMDC2 (paymentMDC2) where

import Prelude ((#), ($), Unit)

import Data.Profunctor.Row.VariantToVariant (iterate)
import Data.Variant (match)
import Effect (Effect)
import PaymentLogic (amountLine, chargeFlaky, recordCharged, retryLine, startCharge, statusLine, unpaidOrder)
import PUI (action, atCase, mvu, observed, toCase, updated)
import PUI.Web (shown, text)
import PUI.Web.MDC2 (body, body2, button, headline6, indeterminateCircularProgress, snackbar)
import QualifiedDo.Semigroupoid as Semigroupoid

paymentMDC2 :: Effect Unit
paymentMDC2 =
  body $
    ( Semigroupoid.do
      ( headline6 $ text amountLine ) # shown
      ( body2 $ text statusLine ) # shown
      ( Semigroupoid.do
        button @"Charge card" { icon: "credit_card" } # toCase @"charge" startCharge
        ( Semigroupoid.do
          indeterminateCircularProgress @"Charging card" # action chargeFlaky # atCase @"charge"
          snackbar @"charge" retryLine # observed ) # iterate ) # updated (match { charged: recordCharged })
    ) # mvu unpaidOrder
