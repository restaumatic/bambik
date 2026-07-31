module PaymentMD3 (paymentMD3) where

import Prelude ((#), ($), (+), (<), (<<<), (<>), Unit, const, discard, pure, show)

import Data.Profunctor (lcmap)
import Data.Profunctor.Row.RecordToRecord as RecordToRecord
import Data.Profunctor.Row.VariantToVariant (iterate)
import Data.Variant (match)
import Effect (Effect)
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff, Milliseconds(..), delay)
import PUI (action, asCase, forField, mvu, onCase, projection, tapped, updates)
import PUI.HTML (body, staticText, text)
import PUI.MDC3 (bodyMedium, button, card, elevation5, headlineSmall, indeterminateCircularProgress)
import QualifiedDo.Semigroupoid as Semigroupoid

paymentMD3 :: Effect Unit
paymentMD3 =
  body $
    elevation5 $
      card { caption: "Payment" } $ ( Semigroupoid.do
          headlineSmall ( RecordToRecord.do
              staticText "Amount due: $"
              text # projection show # forField @"amount" ) # tapped
          bodyMedium text # projection statusLine # tapped
          ( Semigroupoid.do
              button { label: "Charge card", icon: "credit_card" } # asCase @"charge" # lcmap startCharge
              indeterminateCircularProgress # action chargeFlaky # onCase @"charge" # iterate) # updates (match { charged: const <<< recordCharged })
      ) # mvu unpaidOrder

startCharge :: { amount :: Number } -> { amount :: Number, attempt :: Int }
startCharge { amount } = { amount, attempt: 0 }

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
