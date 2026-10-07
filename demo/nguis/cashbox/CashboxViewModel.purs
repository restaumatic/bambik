module CashboxViewModel (balanceLine, courierPaidOutLine, customerRefundedLine, depositTakenLine, openedTill, payCourier, payoutLine, refundLine, refundStandard, takeDeposit) where

import Prelude ((+), (-), (<>), show)

import Data.Maybe (fromMaybe)
import Data.String (Pattern(..), stripSuffix)

openedTill :: { balance :: Number }
openedTill = { balance: 200.0 }

balanceLine :: { balance :: Number } -> String
balanceLine { balance } = "Till balance: €" <> euros balance

refundLine :: { balance :: Number } -> String
refundLine _ = "Hand €" <> euros standardRefund <> " back to the customer."

payoutLine :: { balance :: Number } -> String
payoutLine _ = "Hand €" <> euros courierFee <> " to the courier."

refundStandard :: { balance :: Number } -> { balance :: Number }
refundStandard till = till { balance = till.balance - standardRefund }

payCourier :: { balance :: Number } -> { balance :: Number }
payCourier till = till { balance = till.balance - courierFee }

takeDeposit :: { balance :: Number } -> { balance :: Number }
takeDeposit till = till { balance = till.balance + customerDeposit }

standardRefund :: Number
standardRefund = 25.0

courierFee :: Number
courierFee = 10.0

customerDeposit :: Number
customerDeposit = 50.0

euros :: Number -> String
euros n = let s = show n in fromMaybe s (stripSuffix (Pattern ".0") s)

customerRefundedLine :: { balance :: Number } -> String
customerRefundedLine _ = "Refunded €" <> euros standardRefund <> " to the customer"

courierPaidOutLine :: { balance :: Number } -> String
courierPaidOutLine _ = "Paid €" <> euros courierFee <> " to the courier"

depositTakenLine :: { balance :: Number } -> String
depositTakenLine _ = "Took a €" <> euros customerDeposit <> " deposit"
