module CashboxMDC3 (cashboxMDC3) where

import Prelude (identity, (#), ($), (+), (-), (<>), Unit, show)

import Data.Maybe (fromMaybe)
import Data.Profunctor.Row.RecordToVariant as RecordToVariant
import Data.Profunctor.Row.VariantToVariant as VariantToVariant
import Data.String (Pattern(..), stripSuffix)
import Data.Variant (match)
import Effect (Effect)
import PUI (asCase, focusVariant, mvu, onCase, projected, tapped, toCase, updated, with)
import PUI.HTML (body, text)
import PUI.MDC3 (bodyLarge, bodyMedium, button, card, elevation5, headlineSmall, simpleDialog)
import QualifiedDo.Semigroupoid as Semigroupoid

cashboxMDC3 :: Effect Unit
cashboxMDC3 =
  body $
    elevation5 $
      card { caption: "Cashbox" } $ ( Semigroupoid.do
          headlineSmall text # projected balanceLine # tapped
          bodyMedium text # projected auditLine # tapped
          ( Semigroupoid.do
              RecordToVariant.do
                with standardRefund (button { label: "Refund a customer", icon: "undo" }) # asCase @"refund"
                with courierFee (button { label: "Pay the courier", icon: "local_shipping" }) # asCase @"payout"
                with tillAudit (button { label: "Count the register", icon: "fact_check" }) # asCase @"counted"
              ( VariantToVariant.do
                  simpleDialog { title: "Refund the customer?", confirm: "Refund" } (bodyLarge text # projected refundLine # tapped) # onCase @"refund" # toCase @"refunded" identity
                  simpleDialog { title: "Pay the courier?", confirm: "Pay" } (bodyLarge text # projected payoutLine # tapped) # onCase @"payout" # toCase @"paidOut" identity ) # focusVariant
          ) # updated (match { refunded: applyRefund, paidOut: applyPayout, counted: recordAudit })
      ) # mvu openedTill

applyRefund :: { amount :: Number } -> { balance :: Number, audits :: Int } -> { balance :: Number, audits :: Int }
applyRefund { amount } till = till { balance = till.balance - amount }

applyPayout :: { amount :: Number } -> { balance :: Number, audits :: Int } -> { balance :: Number, audits :: Int }
applyPayout { amount } till = till { balance = till.balance - amount }

recordAudit :: {} -> { balance :: Number, audits :: Int } -> { balance :: Number, audits :: Int }
recordAudit _ till = till { audits = till.audits + 1 }

balanceLine :: { balance :: Number } -> String
balanceLine { balance } = "Till balance: €" <> euros balance

auditLine :: { audits :: Int } -> String
auditLine { audits } = show audits <> " register counts today"

refundLine :: { amount :: Number } -> String
refundLine { amount } = "Hand €" <> euros amount <> " back to the customer."

payoutLine :: { amount :: Number } -> String
payoutLine { amount } = "Hand €" <> euros amount <> " to the courier."

euros :: Number -> String
euros n = fromMaybe shown (stripSuffix (Pattern ".0") shown)
  where
  shown = show n

openedTill :: { balance :: Number, audits :: Int }
openedTill = { balance: 200.0, audits: 0 }

standardRefund :: { amount :: Number }
standardRefund = { amount: 25.0 }

courierFee :: { amount :: Number }
courierFee = { amount: 10.0 }

tillAudit :: {}
tillAudit = {}
