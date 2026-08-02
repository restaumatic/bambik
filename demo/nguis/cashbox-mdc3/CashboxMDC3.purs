module CashboxMDC3 (cashboxMDC3) where

import Prelude (identity, (#), ($), (+), (-), Unit, show)

import Data.Maybe (fromMaybe)
import Data.Profunctor.Row.RecordToRecord as RecordToRecord
import Data.Profunctor.Row.RecordToVariant as RecordToVariant
import Data.Profunctor.Row.VariantToVariant as VariantToVariant
import Data.String (Pattern(..), stripSuffix)
import Data.Variant (match)
import Effect (Effect)
import PUI (asCase, focusVariant, forField, informed, mvu, onCase, tapped, toCase, updated, with)
import PUI.HTML (body, staticText, text)
import PUI.MDC3 (bodyLarge, button, card, elevation5, headlineSmall, simpleDialog)
import QualifiedDo.Semigroupoid as Semigroupoid

cashboxMDC3 :: Effect Unit
cashboxMDC3 =
  body $
    elevation5 $
      card { caption: "Cashbox" } $ ( Semigroupoid.do
          headlineSmall ( RecordToRecord.do
              staticText "Till balance: €"
              text # forField @"balance" euros ) # tapped
          ( Semigroupoid.do
              RecordToVariant.do
                button { label: "Refund a customer", icon: "undo" } # with standardRefund # asCase @"refund"
                button { label: "Pay the courier", icon: "local_shipping" } # with courierFee # asCase @"payout"
                button { label: "Take a deposit", icon: "savings" } # with customerDeposit # asCase @"deposited"
              ( VariantToVariant.do
                  ( simpleDialog { title: "Refund the customer?", confirm: "Refund" } $ bodyLarge ( RecordToRecord.do
                      staticText "Hand €"
                      text # forField @"amount" euros
                      staticText " back to the customer." ) # tapped ) # onCase @"refund" # toCase @"refunded" identity
                  ( simpleDialog { title: "Pay the courier?", confirm: "Pay" } $ bodyLarge ( RecordToRecord.do
                      staticText "Hand €"
                      text # forField @"amount" euros
                      staticText " to the courier." ) # tapped ) # onCase @"payout" # toCase @"paidOut" identity ) # focusVariant) # updated (match { refunded: informed applyRefund, paidOut: informed applyPayout, deposited: informed applyDeposit })
      ) # mvu openedTill

applyRefund :: { amount :: Number, balance :: Number } -> { balance :: Number }
applyRefund { amount, balance } = { balance: balance - amount }

applyPayout :: { amount :: Number, balance :: Number } -> { balance :: Number }
applyPayout { amount, balance } = { balance: balance - amount }

applyDeposit :: { amount :: Number, balance :: Number } -> { balance :: Number }
applyDeposit { amount, balance } = { balance: balance + amount }

euros :: Number -> String
euros n = fromMaybe (show n) (stripSuffix (Pattern ".0") (show n))

openedTill :: { balance :: Number }
openedTill = { balance: 200.0 }

standardRefund :: { amount :: Number }
standardRefund = { amount: 25.0 }

courierFee :: { amount :: Number }
courierFee = { amount: 10.0 }

customerDeposit :: { amount :: Number }
customerDeposit = { amount: 50.0 }
