module PaymentMDC2 (paymentMDC2) where

import Prelude ((#), ($), (+), (<), (<<<), (<>), Unit, const, discard, pure, show)

import Data.Profunctor.Row.RecordToRecord as RecordToRecord
import Data.Profunctor.Row.VariantToVariant (iterate)
import Data.Variant (match)
import Effect (Effect)
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff, Milliseconds(..), delay)
import PUI (action, asCase, toCases, forField, mvu, onCase, projected, tapped, updated)
import PUI.HTML (body, staticText, text)
import PUI.MDC2 (body2, button, card, elevation20, headline6, indeterminateCircularProgress)
import QualifiedDo.Semigroupoid as Semigroupoid

paymentMDC2 :: Effect Unit
paymentMDC2 =
  body $
    elevation20 $
      card { caption: "Payment" } $ ( Semigroupoid.do
          headline6 ( RecordToRecord.do
              staticText "Amount due: $"
              text # projected show # forField @"amount" ) # tapped
          body2 text # projected statusLine # tapped
          ( Semigroupoid.do
              button { label: "Charge card", icon: "credit_card" } # toCases startCharge
              indeterminateCircularProgress # action chargeFlaky # onCase @"charge" # iterate) # updated (match { charged: const <<< recordCharged })
      ) # mvu unpaidOrder

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
