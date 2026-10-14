module Payment.MDC2 (payment) where

import Prelude ((#), ($), identity, Unit)

import Effect (Effect)
import Payment (amountLine, chargeFlaky, statusLine, unpaidOrder)
import PUI (action, atCase, looped, with)
import PUI.Web (text)
import PUI.Web.MDC2 (body, body2, button', headline6, indeterminateCircularProgress, snackbar)
import QualifiedDo.Semigroupoid as Semigroupoid

payment :: Effect Unit
payment =
  body $ Semigroupoid.do
    headline6 $ text amountLine
    body2 $ text statusLine
    button' @"Charge card" { icon: "credit_card" }
    indeterminateCircularProgress # action chargeFlaky # atCase @"Charge card"
    snackbar @"Card charged" identity
  # looped
    @( amount :: Number
     , approval :: [ approved :: { attempt :: Int }, pending :: {} ]
     ) # with unpaidOrder
