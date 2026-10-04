module PaymentViewModel (amountLine, chargeFlaky, retryLine, startCharge, statusLine, unpaidOrder) where

import Prelude (show, (<>), ($), (+), (<), discard, pure)

import Data.Variant (match)
import Effect.Aff (Aff, Milliseconds(..), delay)

unpaidOrder :: { amount :: Number, approval :: [ approved :: { attempt :: Int }, pending :: {} ] }
unpaidOrder = { amount: 42.0, approval: .pending {} }

amountLine :: { amount :: Number, approval :: [ approved :: { attempt :: Int }, pending :: {} ] } -> String
amountLine { amount } = "Amount due: $" <> show amount

statusLine :: { amount :: Number, approval :: [ approved :: { attempt :: Int }, pending :: {} ] } -> String
statusLine { amount, approval } = match
  { pending: \_ -> "Ready to charge — the gateway is flaky, so it retries automatically."
  , approved: \{ attempt } -> "Approved — $" <> show amount <> " charged on attempt " <> show attempt
  } approval

retryLine :: { event :: { amount :: Number, attempt :: Int }, model :: { amount :: Number, approval :: [ approved :: { attempt :: Int }, pending :: {} ] } } -> String
retryLine { event: { attempt } } = "Charge declined — retrying (attempt " <> show attempt <> ")"

startCharge :: [ "Charge card" :: { amount :: Number, approval :: [ approved :: { attempt :: Int }, pending :: {} ] } ] -> { amount :: Number, attempt :: Int }
startCharge = match { "Charge card": \{ amount } -> { amount, attempt: 0 } }

chargeFlaky :: { event :: { amount :: Number, attempt :: Int }, model :: { amount :: Number, approval :: [ approved :: { attempt :: Int }, pending :: {} ] } } -> Aff [ charge :: { event :: { amount :: Number, attempt :: Int }, model :: { amount :: Number, approval :: [ approved :: { attempt :: Int }, pending :: {} ] } }, charged :: { amount :: Number, approval :: [ approved :: { attempt :: Int }, pending :: {} ] } ]
chargeFlaky r@{ event: { attempt }, model } = do
  delay (Milliseconds 700.0)
  let tried = attempt + 1
  pure $ if attempt < 2 then .charge (r { event = r.event { attempt = tried } }) else .charged (recordCharged { attempt: tried } model)

recordCharged :: { attempt :: Int } -> { amount :: Number, approval :: [ approved :: { attempt :: Int }, pending :: {} ] } -> { amount :: Number, approval :: [ approved :: { attempt :: Int }, pending :: {} ] }
recordCharged approved charge = charge { approval = .approved { attempt: approved.attempt } }
