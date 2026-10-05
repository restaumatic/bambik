module PaymentMDC3 (paymentMDC3) where

import Prelude ((#), ($), Unit, identity)

import Effect (Effect)
import PaymentViewModel (amountLine, chargeFlaky, retryLine, startCharge, statusLine, unpaidOrder)
import PUI (action, atCase, cycled, fold, joined, looped, observed, toCase, with)
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
        button @"Charge card" { icon: "credit_card" } # toCase @"charge" @{ amount :: Number, attempt :: Int } startCharge # joined @"charge"
        ( Semigroupoid.do
          indeterminateCircularProgress @"Charging card" # action @[ charged :: { amount :: Number, approval :: [ approved :: { attempt :: Int }, pending :: {} ] }, charge :: { event :: { amount :: Number, attempt :: Int }, model :: { amount :: Number, approval :: [ approved :: { attempt :: Int }, pending :: {} ] } } ] chargeFlaky # atCase @"charge"
          snackbar @"charge" retryLine # observed ) # cycled )
      fold @"charged" identity
    ) # looped @( amount :: Number, approval :: [ approved :: { attempt :: Int }, pending :: {} ] ) # with unpaidOrder
