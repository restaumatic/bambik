module PaymentLogic (chargeFlaky, recordCharged, retryLine, startCharge, statusLine, unpaidOrder) where

import Prelude (show, (<>), ($), (+), (<), discard, pure)

import Data.Maybe (Maybe(..))
import Effect.Aff (Aff, Milliseconds(..), delay)

unpaidOrder :: { amount :: Number, approved :: Maybe { attempt :: Int } }
unpaidOrder = { amount: 42.0, approved: Nothing }

retryLine :: { amount :: Number, attempt :: Int } -> String
retryLine { attempt } = "Charge declined — retrying (attempt " <> show attempt <> ")"

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
