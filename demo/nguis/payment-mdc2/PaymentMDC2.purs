module PaymentMDC2 (paymentMDC2) where

import Prelude ((#), ($), (<<<), Unit, const, show)

import Data.Profunctor.Row.RecordToRecord as RecordToRecord
import Data.Profunctor.Row.VariantToVariant (iterate)
import Data.Variant (match)
import Effect (Effect)
import PaymentLogic (chargeFlaky, recordCharged, retryLine, startCharge, statusLine, unpaidOrder)
import PUI (action, forCase, projection, mvu, observed, atCase, projected, tapped, toCases, updated)
import PUI.Web.HTML (body, staticText, text)
import PUI.Web.MDC2 (body2, button, card, elevation20, headline6, indeterminateCircularProgress, snackbar)
import QualifiedDo.Semigroupoid as Semigroupoid

paymentMDC2 :: Effect Unit
paymentMDC2 =
  body $
    elevation20 $
      card { caption: "Payment" } $ ( Semigroupoid.do
          headline6 ( RecordToRecord.do
              staticText "Amount due: $"
              text @"amount" # projection show ) # tapped
          body2 (text @"status") # projected statusLine # tapped
          ( Semigroupoid.do
              button @"Charge card" { icon: "credit_card" } # toCases startCharge
              ( Semigroupoid.do
                  indeterminateCircularProgress @"busy" # action chargeFlaky # atCase @"charge"
                  snackbar # forCase @"charge" retryLine # observed ) # iterate) # updated (match { charged: const <<< recordCharged })
      ) # mvu unpaidOrder
