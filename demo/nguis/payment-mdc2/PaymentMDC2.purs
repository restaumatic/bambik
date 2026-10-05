module PaymentMDC2 (paymentMDC2) where

import Prelude ((#), ($), Unit, identity)

import Effect (Effect)
import PaymentViewModel (amountLine, chargeFlaky, retryLine, startCharge, statusLine, unpaidOrder)
import PUI (action, atCase, cycled, fold, joined, mvu, observed, toCase)
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
        button @"Charge card" { icon: "credit_card" } # toCase @"charge" @{ amount :: Number, attempt :: Int } startCharge # joined @"charge"
        ( Semigroupoid.do
          indeterminateCircularProgress @"Charging card" # action @[ charged :: { amount :: Number, approval :: [ approved :: { attempt :: Int }, pending :: {} ] }, charge :: { event :: { amount :: Number, attempt :: Int }, model :: { amount :: Number, approval :: [ approved :: { attempt :: Int }, pending :: {} ] } } ] chargeFlaky # atCase @"charge"
          snackbar @"charge" retryLine # observed ) # cycled )
      fold @"charged" identity
    ) # mvu @( amount :: Number, approval :: [ approved :: { attempt :: Int }, pending :: {} ] ) unpaidOrder
