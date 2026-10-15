module Payment.MDC3 (payment) where

import Prelude ((#), ($), identity, Unit)

import Effect (Effect)
import Payment (amountLine, chargeFlaky, statusLine, unpaidOrder)
import PUI (action, looped, with)
import PUI.Web (text)
import PUI.Web.MDC3 (body, bodyMedium, button', headlineSmall, indeterminateCircularProgress, snackbar)
import QualifiedDo.Semigroupoid as Semigroupoid

payment :: Effect Unit
payment =
  body $ Semigroupoid.do
    headlineSmall $ text amountLine
    bodyMedium $ text statusLine
    button' @"Charge card" { icon: "credit_card" }
    indeterminateCircularProgress # action @"Charge card" @( "Card charged" :: _ ) chargeFlaky
    snackbar @"Card charged" identity
  # looped # with unpaidOrder
