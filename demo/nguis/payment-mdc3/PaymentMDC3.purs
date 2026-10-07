module PaymentMDC3 (paymentMDC3) where

import Prelude ((#), ($), Unit, identity)

import Data.Profunctor.Row.VariantToRecord as VariantToRecord
import Effect (Effect)
import PaymentViewModel (amountLine, cardChargedLine, chargeFlaky, chargingLine, statusLine, unpaidOrder)
import PUI (action, atCase, blankStatus, fold, looped, observed, with)
import PUI.Web (shown, text)
import PUI.Web.MDC3 (body, bodyMedium, button, headlineSmall, indeterminateCircularProgress, snackbar)
import QualifiedDo.Semigroupoid as Semigroupoid

paymentMDC3 :: Effect Unit
paymentMDC3 =
  body $
    ( Semigroupoid.do
      ( headlineSmall $ text amountLine ) # shown
      ( bodyMedium $ text statusLine ) # shown
      button @"Charge card" { icon: "credit_card" }
      ( Semigroupoid.do
        snackbar @"Charge card" chargingLine # observed
        ( VariantToRecord.do
          indeterminateCircularProgress
          blankStatus @"Card charged" ) # action chargeFlaky # atCase @"Charge card" )
      snackbar @"Card charged" cardChargedLine # fold identity
    ) # looped @( amount :: Number, approval :: [ approved :: { attempt :: Int }, pending :: {} ] ) # with unpaidOrder
