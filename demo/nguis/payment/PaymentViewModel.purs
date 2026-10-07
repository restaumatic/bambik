module PaymentViewModel (amountLine, cardChargedLine, chargeFlaky, chargingLine, statusLine, unpaidOrder) where

import Prelude (show, (<>), ($), (+), (<), discard, pure)

import Data.Variant (match)
import Effect.Aff (Aff, Milliseconds(..), delay)

unpaidOrder :: { amount :: Number, approval :: [ approved :: { attempt :: Int }, pending :: {} ] }
unpaidOrder = { amount: 42.0, approval: .pending {} }

amountLine
  :: { amount :: Number, approval :: [ approved :: { attempt :: Int }, pending :: {} ] }
  -> String
amountLine { amount } = "Amount due: $" <> show amount

statusLine
  :: { amount :: Number, approval :: [ approved :: { attempt :: Int }, pending :: {} ] }
  -> String
statusLine { amount, approval } = match
  { pending: \_ -> "Ready to charge — the gateway is flaky, so it retries automatically."
  , approved: \{ attempt } -> "Approved — $" <> show amount <> " charged on attempt " <> show attempt
  } approval

chargingLine
  :: { amount :: Number, approval :: [ approved :: { attempt :: Int }, pending :: {} ] }
  -> String
chargingLine { amount } = "Charging $" <> show amount <> " — the gateway is flaky, retrying until approved"

chargeFlaky
  :: { amount :: Number, approval :: [ approved :: { attempt :: Int }, pending :: {} ] }
  -> Aff [ "Card charged" :: { amount :: Number
                             , approval :: [ approved :: { attempt :: Int }, pending :: {} ]
                             }
         ]
chargeFlaky order = attempt 1
  where
  attempt n = do
    delay (Milliseconds 700.0)
    if n < 3 then attempt (n + 1) else pure $ ."Card charged" (recordCharged { attempt: n } order)

recordCharged :: { attempt :: Int } -> { amount :: Number, approval :: [ approved :: { attempt :: Int }, pending :: {} ] } -> { amount :: Number, approval :: [ approved :: { attempt :: Int }, pending :: {} ] }
recordCharged approved charge = charge { approval = .approved { attempt: approved.attempt } }

cardChargedLine
  :: { amount :: Number, approval :: [ approved :: { attempt :: Int }, pending :: {} ] }
  -> String
cardChargedLine { amount, approval } = match
  { pending: \_ -> "Charge of $" <> show amount <> " still pending"
  , approved: \{ attempt } -> "Charged $" <> show amount <> " on attempt " <> show attempt
  } approval
