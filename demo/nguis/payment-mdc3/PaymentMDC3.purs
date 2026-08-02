module PaymentMDC3 (paymentMDC3) where

import Prelude ((#), ($), (+), (<), (<<<), (<>), Unit, const, discard, pure, show)

import Data.Profunctor.Row.RecordToRecord as RecordToRecord
import Data.Profunctor.Row.VariantToVariant (iterate)
import Data.Variant (match)
import Effect (Effect)
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff, Milliseconds(..), delay)
import PUI (PUI, action, forCase, forField, mvu, observed, onCase, projected, tapped, toCases, updated)
import PUI.Web.HTML (body, staticText, text)
import PUI.Web (Web)
import PUI.Web.MDC3 (bodyMedium, button, card, elevation5, headlineSmall, indeterminateCircularProgress, snackbar)
import QualifiedDo.Semigroupoid as Semigroupoid

paymentMDC3 :: Effect Unit
paymentMDC3 =
  body $
    elevation5 $
      card { caption: "Payment" } $ ( Semigroupoid.do
          headlineSmall ( RecordToRecord.do
              staticText "Amount due: $"
              text # forField @"amount" show ) # tapped
          bodyMedium text # projected statusLine # tapped
          ( Semigroupoid.do
              button { label: "Charge card", icon: "credit_card" } # toCases startCharge
              ( Semigroupoid.do
                  indeterminateCircularProgress # action chargeFlaky # onCase @"charge"
                  retryToast ) # iterate) # updated (match { charged: const <<< recordCharged })
      ) # mvu unpaidOrder

retryToast :: PUI Web [ charged :: { attempt :: Int }, charge :: { amount :: Number, attempt :: Int } ] [ charged :: { attempt :: Int }, charge :: { amount :: Number, attempt :: Int } ]
retryToast = snackbar # forCase @"charge" (\{ attempt } -> "Charge declined — retrying (attempt " <> show attempt <> ")") # observed

startCharge :: { amount :: Number } -> [ charge :: { amount :: Number, attempt :: Int } ]
startCharge { amount } = .charge { amount, attempt: 0 }

chargeFlaky :: { amount :: Number, attempt :: Int } -> Aff
  [ charged :: { attempt :: Int }
  , charge :: { amount :: Number, attempt :: Int }
  ]
chargeFlaky r@{ attempt } = do
  delay (Milliseconds 700.0)
  if attempt < 2
    then pure $ .charge r { attempt = attempt + 1 }
    else pure $ .charged { attempt: attempt + 1 }

recordCharged :: { attempt :: Int } -> { approved :: Maybe { attempt :: Int } }
recordCharged approval = { approved: Just approval }

statusLine :: { amount :: Number, approved :: Maybe { attempt :: Int } } -> String
statusLine { amount, approved } = case approved of
  Nothing -> "Ready to charge — the gateway is flaky, so it retries automatically."
  Just { attempt } -> "Approved — $" <> show amount <> " charged on attempt " <> show attempt

unpaidOrder :: { amount :: Number, approved :: Maybe { attempt :: Int } }
unpaidOrder = { amount: 42.0, approved: Nothing }
