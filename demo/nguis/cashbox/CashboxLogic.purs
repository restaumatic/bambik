module CashboxLogic (applyDeposit, applyPayout, applyRefund, courierFee, customerDeposit, openedTill, payoutSum, presentCashbox, refundSum, standardRefund) where

import Prelude ((+), (-), (<>), show)

import Data.Maybe (fromMaybe)
import Data.String (Pattern(..), stripSuffix)

openedTill :: { balance :: Number, balanceLine :: String }
openedTill = presentCashbox { balance: 200.0, balanceLine: "" }

presentCashbox :: { balance :: Number, balanceLine :: String } -> { balance :: Number, balanceLine :: String }
presentCashbox r = r { balanceLine = "Till balance: €" <> euros r.balance }

standardRefund :: { amount :: Number, refundLine :: String }
standardRefund = { amount: 25.0, refundLine: "Hand €" <> euros 25.0 <> " back to the customer." }

courierFee :: { amount :: Number, payoutLine :: String }
courierFee = { amount: 10.0, payoutLine: "Hand €" <> euros 10.0 <> " to the courier." }

customerDeposit :: { amount :: Number }
customerDeposit = { amount: 50.0 }

refundSum :: { amount :: Number, refundLine :: String } -> { amount :: Number }
refundSum { amount } = { amount }

payoutSum :: { amount :: Number, payoutLine :: String } -> { amount :: Number }
payoutSum { amount } = { amount }

applyRefund :: { amount :: Number } -> { balance :: Number } -> { balance :: Number }
applyRefund { amount } { balance } = { balance: balance - amount }

applyPayout :: { amount :: Number } -> { balance :: Number } -> { balance :: Number }
applyPayout { amount } { balance } = { balance: balance - amount }

applyDeposit :: { amount :: Number } -> { balance :: Number } -> { balance :: Number }
applyDeposit { amount } { balance } = { balance: balance + amount }

euros :: Number -> String
euros n = fromMaybe (show n) (stripSuffix (Pattern ".0") (show n))
