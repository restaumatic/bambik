module Payment.MDC3 (payment) where

import Prelude ((#), ($), Unit)

import Effect (Effect)
import Payment (amountLine, charged, chargeFlaky, statusLine)
import PUI (action, told)
import PUI.Web (providedAt, text)
import PUI.Web.MDC3 (body, bodyMedium, button', headlineSmall, indeterminateCircularProgress, snackbar)
import QualifiedDo.Semigroupoid as Semigroupoid

payment :: Effect Unit
payment =
  body $ Semigroupoid.do
    headlineSmall $ text amountLine
    bodyMedium $ text statusLine
    button' @"Charge card" { icon: "credit_card" } # providedAt @"pending" @"approval"
    indeterminateCircularProgress # action @"Charge card" chargeFlaky
    snackbar @"Card charged" # told
  # charged
