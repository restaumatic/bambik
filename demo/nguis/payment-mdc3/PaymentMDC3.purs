module PaymentMDC3 (paymentMDC3) where

import Prelude ((#), ($), Unit, identity)

import Effect (Effect)
import PUI.Pipeline as Pipeline
import PaymentViewModel (amountLine, cardChargedLine, chargeFlaky, chargingLine, statusLine, unpaidOrder)
import PUI (action, atCase, fold, looped, observed, with)
import PUI.Web (text)
import PUI.Web.MDC3 (body, bodyMedium, button, headlineSmall, indeterminateCircularProgress, snackbar)

paymentMDC3 :: Effect Unit
paymentMDC3 =
  body $ Pipeline.do
    headlineSmall $ text amountLine
    bodyMedium $ text statusLine
    button @"Charge card" { icon: "credit_card" }
    ( Pipeline.do
      snackbar @"Charge card" chargingLine # observed
      indeterminateCircularProgress # action chargeFlaky # atCase @"Charge card" )
    snackbar @"Card charged" cardChargedLine # fold identity
  # looped
    @( amount :: Number
     , approval :: [ approved :: { attempt :: Int }, pending :: {} ]
     ) # with unpaidOrder
