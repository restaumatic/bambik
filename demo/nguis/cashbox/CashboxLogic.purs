module CashboxLogic (applyDeposit, applyPayout, applyRefund, balanceLine, courierFee, customerDeposit, openedTill, payoutLine, refundLine, standardRefund) where

import Prelude ((+), (-), (<>), show)

import Data.Maybe (fromMaybe)
import Data.String (Pattern(..), stripSuffix)

openedTill :: { balance :: Number }
openedTill = { balance: 200.0 }

balanceLine :: { balance :: Number } -> String
balanceLine { balance } = "Till balance: €" <> euros balance

standardRefund :: { amount :: Number }
standardRefund = { amount: 25.0 }

courierFee :: { amount :: Number }
courierFee = { amount: 10.0 }

customerDeposit :: { amount :: Number }
customerDeposit = { amount: 50.0 }

refundLine :: { amount :: Number } -> String
refundLine { amount } = "Hand €" <> euros amount <> " back to the customer."

payoutLine :: { amount :: Number } -> String
payoutLine { amount } = "Hand €" <> euros amount <> " to the courier."

applyRefund :: { amount :: Number } -> { balance :: Number } -> { balance :: Number }
applyRefund { amount } { balance } = { balance: balance - amount }

applyPayout :: { amount :: Number } -> { balance :: Number } -> { balance :: Number }
applyPayout { amount } { balance } = { balance: balance - amount }

applyDeposit :: { amount :: Number } -> { balance :: Number } -> { balance :: Number }
applyDeposit { amount } { balance } = { balance: balance + amount }

euros :: Number -> String
euros n = fromMaybe (show n) (stripSuffix (Pattern ".0") (show n))
