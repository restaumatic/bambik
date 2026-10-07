module PaymentMDC2 (paymentMDC2) where

import Prelude ((#), ($), Unit, identity)

import Data.Profunctor.Row.VariantToRecord as VariantToRecord
import Effect (Effect)
import PaymentViewModel (amountLine, cardChargedLine, chargeFlaky, chargingLine, statusLine, unpaidOrder)
import PUI (action, atCase, blankStatus, fold, looped, observed, with)
import PUI.Web (shown, text)
import PUI.Web.MDC2 (body, body2, button, headline6, indeterminateCircularProgress, snackbar)
import QualifiedDo.Semigroupoid as Semigroupoid

paymentMDC2 :: Effect Unit
paymentMDC2 =
  body $
    ( Semigroupoid.do
      ( headline6 $ text amountLine ) # shown
      ( body2 $ text statusLine ) # shown
      button @"Charge card" { icon: "credit_card" }
      ( Semigroupoid.do
        snackbar @"Charge card" chargingLine # observed
        ( VariantToRecord.do
          indeterminateCircularProgress
          blankStatus @"Card charged" ) # action chargeFlaky # atCase @"Charge card" )
      snackbar @"Card charged" cardChargedLine # fold identity
    ) # looped @( amount :: Number, approval :: [ approved :: { attempt :: Int }, pending :: {} ] ) # with unpaidOrder
