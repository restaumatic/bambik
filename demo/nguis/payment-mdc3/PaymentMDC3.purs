module PaymentMDC3 (paymentMDC3) where

import Prelude (identity, (#), ($), (<<<), Unit, const, show)

import Data.Profunctor.Row.RecordToRecord as RecordToRecord
import Data.Profunctor.Row.VariantToVariant (iterate)
import Data.Variant (match)
import Effect (Effect)
import PaymentLogic (chargeFlaky, recordCharged, retryLine, startCharge, statusLine, unpaidOrder)
import PUI (action, forCase, projection, mvu, observed, atCase, projected, toCases, updated)
import PUI.Web.HTML (shownAs, body, staticText, text)
import PUI.Web.MDC3 (bodyMedium, button, card, elevation5, headlineSmall, indeterminateCircularProgress, snackbar)
import QualifiedDo.Semigroupoid as Semigroupoid

paymentMDC3 :: Effect Unit
paymentMDC3 =
  body $
    elevation5 $
      card $ ( Semigroupoid.do
          ( headlineSmall $ RecordToRecord.do
              staticText "Amount due: $"
              text @"amount" # projection show ) # shownAs identity
          (bodyMedium (text @"status") # projected statusLine) # shownAs identity
          ( Semigroupoid.do
              button @"Charge card" { icon: "credit_card" } # toCases startCharge
              ( Semigroupoid.do
                  indeterminateCircularProgress @"busy" # action chargeFlaky # atCase @"charge"
                  snackbar # forCase @"charge" retryLine # observed ) # iterate ) # updated (match { charged: const <<< recordCharged })
      ) # mvu unpaidOrder
