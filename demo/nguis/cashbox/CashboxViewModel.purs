module CashboxViewModel (applyDeposit, applyPayout, applyRefund, balanceLine, courierFee, customerDeposit, openedTill, payoutLine, refundLine, standardRefund) where

import Prelude ((+), (-), (<>), show)

import Data.Maybe (fromMaybe)
import Data.String (Pattern(..), stripSuffix)

openedTill :: { balance :: Number }
openedTill = { balance: 200.0 }

balanceLine :: forall r1. { balance :: Number | r1 } -> String
balanceLine { balance } = "Till balance: €" <> euros balance

standardRefund :: { amount :: Number }
standardRefund = { amount: 25.0 }

courierFee :: { amount :: Number }
courierFee = { amount: 10.0 }

customerDeposit :: { amount :: Number }
customerDeposit = { amount: 50.0 }

refundLine :: forall r1. { amount :: Number | r1 } -> String
refundLine { amount } = "Hand €" <> euros amount <> " back to the customer."

payoutLine :: forall r1. { amount :: Number | r1 } -> String
payoutLine { amount } = "Hand €" <> euros amount <> " to the courier."

applyRefund :: forall r1 r2. { amount :: Number | r1 } -> { balance :: Number | r2 } -> { balance :: Number | r2 }
applyRefund { amount } till = till { balance = till.balance - amount }

applyPayout :: forall r1 r2. { amount :: Number | r1 } -> { balance :: Number | r2 } -> { balance :: Number | r2 }
applyPayout { amount } till = till { balance = till.balance - amount }

applyDeposit :: forall r1 r2. { amount :: Number | r1 } -> { balance :: Number | r2 } -> { balance :: Number | r2 }
applyDeposit { amount } till = till { balance = till.balance + amount }

euros :: Number -> String
euros n = let s = show n in fromMaybe s (stripSuffix (Pattern ".0") s)
