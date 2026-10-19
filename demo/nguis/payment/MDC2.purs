module Payment.MDC2 (payment) where

import Prelude ((#), ($), identity, Unit)

import Effect (Effect)
import Payment (amountLine, chargeFlaky, statusLine, unpaidOrder)
import PUI (action, looped, with)
import PUI.Web (providedAt, text)
import PUI.Web.MDC2 (body, body2, button', headline6, indeterminateCircularProgress, snackbar)
import QualifiedDo.Semigroupoid as Semigroupoid

payment :: Effect Unit
payment =
  body $ Semigroupoid.do
    headline6 $ text amountLine
    body2 $ text statusLine
    button' @"Charge card" { icon: "credit_card" } # providedAt @"pending" @"approval"
    indeterminateCircularProgress # action @"Charge card" chargeFlaky
    snackbar @"Card charged" identity
  # looped # with unpaidOrder
