module CashboxLogic (applyDeposit, applyPayout, applyRefund, courierFee, customerDeposit, euros, openedTill, standardRefund) where

import Prelude ((+), (-), show)

import Data.Maybe (fromMaybe)
import Data.String (Pattern(..), stripSuffix)

openedTill :: { balance :: Number }
openedTill = { balance: 200.0 }

standardRefund :: { amount :: Number }
standardRefund = { amount: 25.0 }

courierFee :: { amount :: Number }
courierFee = { amount: 10.0 }

customerDeposit :: { amount :: Number }
customerDeposit = { amount: 50.0 }

applyRefund :: { amount :: Number } -> { balance :: Number } -> { balance :: Number }
applyRefund { amount } { balance } = { balance: balance - amount }

applyPayout :: { amount :: Number } -> { balance :: Number } -> { balance :: Number }
applyPayout { amount } { balance } = { balance: balance - amount }

applyDeposit :: { amount :: Number } -> { balance :: Number } -> { balance :: Number }
applyDeposit { amount } { balance } = { balance: balance + amount }

euros :: Number -> String
euros n = fromMaybe (show n) (stripSuffix (Pattern ".0") (show n))
