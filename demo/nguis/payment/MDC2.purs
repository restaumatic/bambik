module Payment.MDC2 (payment) where

import Prelude ((#), ($), Unit)

import Effect (Effect)
import Payment (amountLine, chargeFlaky, statusLine, unpaidOrder)
import PUI (action, captured, looped, stepped, with)
import PUI.Web (providedAt, text)
import PUI.Web.MDC2 (body, body2, button', headline6, indeterminateCircularProgress, snackbar)
import QualifiedDo.Semigroupoid as Semigroupoid

payment :: Effect Unit
payment =
  body $ Semigroupoid.do
    headline6 $ text amountLine
    body2 $ text statusLine
    ( Semigroupoid.do
      button' @"Charge card" { icon: "credit_card" } # providedAt @"pending" @"approval"
      indeterminateCircularProgress # action @"Charge card" chargeFlaky
      snackbar @"Card charged" captured ) # stepped
  # looped # with unpaidOrder
