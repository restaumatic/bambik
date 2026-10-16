module Cashbox (Till, balanceLine, openedTill, payCourier, payoutLine, refundLine, refundStandard, takeDeposit) where

import Prelude ((+), (-), (<>), show)

import Data.Maybe (fromMaybe)
import Data.String (Pattern(..), stripSuffix)

type Till = { balance :: Number }

openedTill :: Till
openedTill = { balance: 200.0 }

balanceLine :: Till -> String
balanceLine { balance } = "Till balance: €" <> euros balance

refundLine :: Till -> String
refundLine _ = "Hand €" <> euros standardRefund <> " back to the customer."

payoutLine :: Till -> String
payoutLine _ = "Hand €" <> euros courierFee <> " to the courier."

refundStandard :: Till -> Till
refundStandard till = till { balance = till.balance - standardRefund }

payCourier :: Till -> Till
payCourier till = till { balance = till.balance - courierFee }

takeDeposit :: Till -> Till
takeDeposit till = till { balance = till.balance + customerDeposit }

standardRefund :: Number
standardRefund = 25.0

courierFee :: Number
courierFee = 10.0

customerDeposit :: Number
customerDeposit = 50.0

euros :: Number -> String
euros n = let s = show n in fromMaybe s (stripSuffix (Pattern ".0") s)
