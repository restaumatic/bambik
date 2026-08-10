module PaymentMDC3 (paymentMDC3) where

import Prelude ((#), ($), (<<<), Unit, const, show)

import Data.Profunctor.Row.RecordToRecord as RecordToRecord
import Data.Profunctor.Row.VariantToVariant (iterate)
import Data.Variant (match)
import Effect (Effect)
import PaymentLogic (chargeFlaky, recordCharged, retryLine, startCharge, statusLine, unpaidOrder)
import PUI (action, forCase, forField, mvu, observed, atCase, projected, tapped, toCases, updated)
import PUI.Web.HTML (body, staticText, text)
import PUI.Web.MDC3 (bodyMedium, button, card, elevation5, headlineSmall, indeterminateCircularProgress, snackbar)
import QualifiedDo.Semigroupoid as Semigroupoid

paymentMDC3 :: Effect Unit
paymentMDC3 =
  body $
    elevation5 $
      card { caption: "Payment" } $ ( Semigroupoid.do
          headlineSmall ( RecordToRecord.do
              staticText "Amount due: $"
              text # forField @"value" @"amount" show ) # tapped
          bodyMedium text # projected @"value" statusLine # tapped
          ( Semigroupoid.do
              button { label: "Charge card", icon: "credit_card" } # toCases @"clicked" startCharge
              ( Semigroupoid.do
                  indeterminateCircularProgress # action chargeFlaky # atCase @"charge"
                  snackbar # forCase @"event" @"charge" retryLine # observed ) # iterate) # updated (match { charged: const <<< recordCharged })
      ) # mvu unpaidOrder
