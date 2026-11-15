module Payment.MDC3 (payment) where

import Prelude ((#), ($), Unit)

import Effect (Effect)
import Payment (amountLine, chargeFlaky, statusLine, unpaidOrder)
import PUI (action, captured, looped, stepped, with)
import PUI.Web (providedAt, text)
import PUI.Web.MDC3 (body, bodyMedium, button', headlineSmall, indeterminateCircularProgress, snackbar)
import QualifiedDo.Semigroupoid as Semigroupoid

payment :: Effect Unit
payment =
  body $ Semigroupoid.do
    headlineSmall $ text amountLine
    bodyMedium $ text statusLine
    ( Semigroupoid.do
      button' @"Charge card" { icon: "credit_card" } # providedAt @"pending" @"approval"
      indeterminateCircularProgress # action @"Charge card" chargeFlaky
      snackbar @"Card charged" captured ) # stepped
  # looped # with unpaidOrder
