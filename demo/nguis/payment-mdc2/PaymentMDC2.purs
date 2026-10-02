module PaymentMDC2 (paymentMDC2) where

import Prelude ((#), ($), Unit)

import Data.Profunctor.Row.VariantToVariant (iterate)
import Data.Variant (match)
import Effect (Effect)
import PaymentViewModel (amountLine, chargeFlaky, recordCharged, retryLine, startCharge, statusLine, unpaidOrder)
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
        button @"Charge card" { icon: "credit_card" } # toCase @"charge" @{ amount :: Number, attempt :: Int } startCharge
        ( Semigroupoid.do
          indeterminateCircularProgress @"Charging card" # action @[ charged :: { attempt :: Int } , charge :: { amount :: Number, attempt :: Int } ] chargeFlaky # atCase @"charge"
          snackbar @"charge" retryLine # observed ) # iterate ) # updated (match { charged: recordCharged })
    ) # mvu @( amount :: Number, approval :: [ approved :: { attempt :: Int }, pending :: {} ] ) unpaidOrder
