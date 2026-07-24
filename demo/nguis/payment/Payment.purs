module Payment (payment) where

import Prelude ((#), ($), (+), (<), (<>), Unit, bind, discard, pure, show)

import Data.Profunctor (lcmap)
import Data.Profunctor.Row.VariantToVariant (iterate)
import Data.Variant (match)
import Effect (Effect)
import Effect.Aff (Aff, Milliseconds(..), delay)
import PUI (action, asCase, mvu, onCase, projection, tapped, updates)
import PUI.HTML (body, text)
import PUI.MDC (body2, button, card, elevation20, headline6, indeterminateCircularProgress)
import QualifiedDo.Semigroupoid as Semigroupoid

payment :: Effect Unit
payment =
  body $
    elevation20 $
      card { caption: "Payment" } $ ( Semigroupoid.do
          headline6 text # projection amountLine # tapped
          body2 text # projection _.status # tapped
          ( Semigroupoid.do
              button { label: "Charge card", icon: "credit_card" } # asCase @"charge" # lcmap startCharge
              indeterminateCircularProgress # action chargeFlaky # onCase @"charge" # iterate) # updates (match { charged: recordCharged })
      ) # mvu unpaidOrder

startCharge :: forall r. { amount :: Number | r } -> { amount :: Number, attempt :: Int }
startCharge o = { amount: o.amount, attempt: 0 }

chargeFlaky :: { amount :: Number, attempt :: Int } -> Aff
  [ charged :: String
  , charge :: { amount :: Number, attempt :: Int }
  ]
chargeFlaky r = do
  delay (Milliseconds 700.0)
  if r.attempt < 2
    then pure $ .charge r { attempt = r.attempt + 1 }
    else pure $ .charged ("Approved — $" <> show r.amount <> " charged on attempt " <> show (r.attempt + 1))

recordCharged :: forall r. String -> { status :: String | r } -> { status :: String | r }
recordCharged message o = o { status = message }

amountLine :: forall r. { amount :: Number | r } -> String
amountLine o = "Amount due: $" <> show o.amount

unpaidOrder :: { amount :: Number, status :: String }
unpaidOrder = { amount: 42.0, status: "Ready to charge — the gateway is flaky, so it retries automatically." }
