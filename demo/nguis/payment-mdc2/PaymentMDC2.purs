module PaymentMDC2 (paymentMDC2) where

import Prelude ((#), ($), Unit, identity)

import Effect (Effect)
import PUI.Pipeline as Pipeline
import PaymentViewModel (amountLine, cardChargedLine, chargeFlaky, chargingLine, statusLine, unpaidOrder)
import PUI (action, atCase, fold, looped, observed, with)
import PUI.Web (text)
import PUI.Web.MDC2 (body, body2, button, headline6, indeterminateCircularProgress, snackbar)

paymentMDC2 :: Effect Unit
paymentMDC2 =
  body $ Pipeline.do
    headline6 $ text amountLine
    body2 $ text statusLine
    button @"Charge card" { icon: "credit_card" }
    ( Pipeline.do
      snackbar @"Charge card" chargingLine # observed
      indeterminateCircularProgress # action chargeFlaky # atCase @"Charge card" )
    snackbar @"Card charged" cardChargedLine # fold identity
  # looped
    @( amount :: Number
     , approval :: [ approved :: { attempt :: Int }, pending :: {} ]
     ) # with unpaidOrder
