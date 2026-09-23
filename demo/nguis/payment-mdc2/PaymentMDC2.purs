module PaymentMDC2 (paymentMDC2) where

import Prelude ((#), ($), (<<<), Unit, const)

import Data.Profunctor.Row.VariantToVariant (iterate)
import Data.Variant (match)
import Effect (Effect)
import PaymentLogic (amountLine, chargeFlaky, recordCharged, retryLine, startCharge, statusLine, unpaidOrder)
import PUI (action, atCase, forCase, mvu, observed, toCases, updated)
import PUI.Web.HTML (shown, text)
import PUI.Web.MDC2 (body, body2, button, card, headline6, indeterminateCircularProgress, snackbar)
import QualifiedDo.Semigroupoid as Semigroupoid

paymentMDC2 :: Effect Unit
paymentMDC2 =
  body $
    card $ ( Semigroupoid.do
      ( headline6 $ text amountLine ) # shown
      ( body2 $ text statusLine ) # shown
      ( Semigroupoid.do
        button @"Charge card" { icon: "credit_card" } # toCases startCharge
        ( Semigroupoid.do
          indeterminateCircularProgress @"busy" # action chargeFlaky # atCase @"charge"
          snackbar # forCase @"charge" retryLine # observed ) # iterate ) # updated (match { charged: const <<< recordCharged })
    ) # mvu unpaidOrder
