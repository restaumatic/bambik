module CashboxLogic (applyDeposit, applyPayout, applyRefund, courierFee, customerDeposit, openedTill, presentCashbox, standardRefund) where

import Prelude ((+), (-), show)

import Data.Maybe (fromMaybe)
import Data.String (Pattern(..), stripSuffix)

openedTill :: { balance :: Number, balanceText :: String }
openedTill = presentCashbox { balance: 200.0, balanceText: "" }

presentCashbox :: { balance :: Number, balanceText :: String } -> { balance :: Number, balanceText :: String }
presentCashbox r = r { balanceText = euros r.balance }

standardRefund :: { amount :: Number, amountText :: String }
standardRefund = cashSum 25.0

courierFee :: { amount :: Number, amountText :: String }
courierFee = cashSum 10.0

customerDeposit :: { amount :: Number }
customerDeposit = { amount: 50.0 }

applyRefund :: { amount :: Number, amountText :: String } -> { balance :: Number } -> { balance :: Number }
applyRefund { amount } { balance } = { balance: balance - amount }

applyPayout :: { amount :: Number, amountText :: String } -> { balance :: Number } -> { balance :: Number }
applyPayout { amount } { balance } = { balance: balance - amount }

applyDeposit :: { amount :: Number } -> { balance :: Number } -> { balance :: Number }
applyDeposit { amount } { balance } = { balance: balance + amount }

cashSum :: Number -> { amount :: Number, amountText :: String }
cashSum n = { amount: n, amountText: euros n }

euros :: Number -> String
euros n = fromMaybe (show n) (stripSuffix (Pattern ".0") (show n))
