module Payment.MDC3 (payment) where

import Prelude ((#), ($), identity, Unit)

import Effect (Effect)
import Payment (amountLine, cardChargedLine, chargeFlaky, chargingLine, statusLine, unpaidOrder)
import PUI (action, atCase, looped, observed, with)
import PUI.Web (text)
import PUI.Web.MDC3 (body, bodyMedium, button', headlineSmall, indeterminateCircularProgress, snackbar)
import QualifiedDo.Semigroupoid as Semigroupoid

payment :: Effect Unit
payment =
  body $ Semigroupoid.do
    headlineSmall $ text amountLine
    bodyMedium $ text statusLine
    button' @"Charge card" { icon: "credit_card" }
    ( Semigroupoid.do
      snackbar @"Charge card" chargingLine identity # observed
      indeterminateCircularProgress # action chargeFlaky # atCase @"Charge card" )
    snackbar @"Card charged" cardChargedLine identity
  # looped
    @( amount :: Number
     , approval :: [ approved :: { attempt :: Int }, pending :: {} ]
     ) # with unpaidOrder
