module PaymentViewModel (amountLine, chargeFlaky, recordCharged, retryLine, startCharge, statusLine, unpaidOrder) where

import Prelude (show, (<>), ($), (+), (<), discard, pure)

import Data.Variant (match)
import Effect.Aff (Aff, Milliseconds(..), delay)

unpaidOrder :: { amount :: Number, approval :: [ approved :: { attempt :: Int }, pending :: {} ] }
unpaidOrder = { amount: 42.0, approval: .pending {} }

amountLine :: forall r1. { amount :: Number | r1 } -> String
amountLine { amount } = "Amount due: $" <> show amount

statusLine :: forall r1. { amount :: Number, approval :: [ approved :: { attempt :: Int }, pending :: {} ] | r1 } -> String
statusLine { amount, approval } = match
  { pending: \_ -> "Ready to charge — the gateway is flaky, so it retries automatically."
  , approved: \{ attempt } -> "Approved — $" <> show amount <> " charged on attempt " <> show attempt
  } approval

retryLine :: forall r1. { amount :: Number, attempt :: Int | r1 } -> String
retryLine { attempt } = "Charge declined — retrying (attempt " <> show attempt <> ")"

startCharge :: forall r. [ "Charge card" :: { amount :: Number | r } ] -> { amount :: Number, attempt :: Int }
startCharge = match { "Charge card": \{ amount } -> { amount, attempt: 0 } }

chargeFlaky :: forall r1. { amount :: Number, attempt :: Int | r1 } -> Aff [ charged :: { attempt :: Int } , charge :: { amount :: Number, attempt :: Int } ]
chargeFlaky r@{ attempt } = do
  delay (Milliseconds 700.0)
  let tried = attempt + 1
  pure $ if attempt < 2 then .charge { amount: r.amount, attempt: tried } else .charged { attempt: tried }

recordCharged :: forall r1 r2. { attempt :: Int | r1 } -> { approval :: [ approved :: { attempt :: Int }, pending :: {} ] | r2 } -> { approval :: [ approved :: { attempt :: Int }, pending :: {} ] | r2 }
recordCharged approved charge = charge { approval = .approved { attempt: approved.attempt } }
