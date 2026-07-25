module Payment (payment) where

import Prelude ((#), ($), (+), (<), (<>), Unit, bind, discard, pure, show)

import Data.Profunctor (lcmap)
import Data.Profunctor.Row.RecordToRecord as RecordToRecord
import Data.Profunctor.Row.VariantToVariant (iterate)
import Data.Variant (match)
import Effect (Effect)
import Effect.Aff (Aff, Milliseconds(..), delay)
import PUI (action, asCase, forField, forValue, mvu, onCase, projection, tapped, updates)
import PUI.HTML (body, staticText, text)
import PUI.MDC (body2, button, card, elevation20, headline6, indeterminateCircularProgress)
import QualifiedDo.Semigroupoid as Semigroupoid

payment :: Effect Unit
payment =
  body $
    elevation20 $
      card { caption: "Payment" } $ ( Semigroupoid.do
          headline6 ( RecordToRecord.do
              staticText "Amount due: $"
              text # projection show # forField @"amount" ) # tapped
          body2 text # forValue # forField @"status" # tapped
          ( Semigroupoid.do
              button { label: "Charge card", icon: "credit_card" } # asCase @"charge" # lcmap startCharge
              indeterminateCircularProgress # action chargeFlaky # onCase @"charge" # iterate) # updates (match { charged: recordCharged })
      ) # mvu unpaidOrder

startCharge :: { amount :: Number } -> { amount :: Number, attempt :: Int }
startCharge { amount } = { amount, attempt: 0 }

chargeFlaky :: { amount :: Number, attempt :: Int } -> Aff
  [ charged :: String
  , charge :: { amount :: Number, attempt :: Int }
  ]
chargeFlaky r@{ amount, attempt } = do
  delay (Milliseconds 700.0)
  if attempt < 2
    then pure $ .charge r { attempt = attempt + 1 }
    else pure $ .charged ("Approved — $" <> show amount <> " charged on attempt " <> show (attempt + 1))

recordCharged :: String -> { status :: String } -> { status :: String }
recordCharged message o = o { status = message }

unpaidOrder :: { amount :: Number, status :: String }
unpaidOrder = { amount: 42.0, status: "Ready to charge — the gateway is flaky, so it retries automatically." }
