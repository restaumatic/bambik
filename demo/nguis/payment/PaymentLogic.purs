module PaymentLogic (chargeFlaky, presentPayment, recordCharged, retryLine, startCharge, unpaidOrder) where

import Prelude (show, (<>), ($), (+), (<), discard, pure)

import Data.Variant (match)
import Effect.Aff (Aff, Milliseconds(..), delay)

unpaidOrder :: { amount :: Number, approval :: [ approved :: { attempt :: Int }, pending :: {} ], amountLine :: String, statusText :: String }
unpaidOrder = presentPayment { amount: 42.0, approval: .pending {}, amountLine: "", statusText: "" }

presentPayment :: { amount :: Number, approval :: [ approved :: { attempt :: Int }, pending :: {} ], amountLine :: String, statusText :: String } -> { amount :: Number, approval :: [ approved :: { attempt :: Int }, pending :: {} ], amountLine :: String, statusText :: String }
presentPayment r = r
  { amountLine = "Amount due: $" <> show r.amount
  , statusText = match
      { pending: \_ -> "Ready to charge — the gateway is flaky, so it retries automatically."
      , approved: \{ attempt } -> "Approved — $" <> show r.amount <> " charged on attempt " <> show attempt
      } r.approval
  }

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

recordCharged :: { attempt :: Int } -> { approval :: [ approved :: { attempt :: Int }, pending :: {} ] }
recordCharged approved = { approval: .approved approved }
