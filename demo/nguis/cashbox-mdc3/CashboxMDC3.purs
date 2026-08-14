module CashboxMDC3 (cashboxMDC3) where

import Prelude (identity, (#), ($), Unit)

import CashboxLogic (applyDeposit, applyPayout, applyRefund, courierFee, customerDeposit, euros, openedTill, standardRefund)
import Data.Profunctor.Row.RecordToRecord as RecordToRecord
import Data.Profunctor.Row.RecordToVariant as RecordToVariant
import Data.Profunctor.Row.VariantToVariant as VariantToVariant
import Data.Variant (match)
import Effect (Effect)
import PUI (atCase, projection, informed, mvu, subChoice, tapped, toCase, updated, with)
import PUI.Web.HTML (body, staticText, text)
import PUI.Web.MDC3 (bodyLarge, button, card, elevation5, headlineSmall, simpleDialog)
import QualifiedDo.Semigroupoid as Semigroupoid

cashboxMDC3 :: Effect Unit
cashboxMDC3 =
  body $
    elevation5 $
      card { caption: "Cashbox" } $ ( Semigroupoid.do
          headlineSmall ( RecordToRecord.do
              staticText "Till balance: €"
              text @"balance" # projection euros ) # tapped
          ( Semigroupoid.do
              RecordToVariant.do
                button @"Refund a customer" { label: "Refund a customer", icon: "undo" } # with standardRefund
                button @"Pay the courier" { label: "Pay the courier", icon: "local_shipping" } # with courierFee
                button @"Take a deposit" { label: "Take a deposit", icon: "savings" } # with customerDeposit
              ( VariantToVariant.do
                  ( simpleDialog { title: "Refund the customer?", confirm: "Refund" } $ bodyLarge ( RecordToRecord.do
                      staticText "Hand €"
                      text @"amount" # projection euros
                      staticText " back to the customer." ) # tapped ) # atCase @"Refund a customer" # toCase @"refunded" identity
                  ( simpleDialog { title: "Pay the courier?", confirm: "Pay" } $ bodyLarge ( RecordToRecord.do
                      staticText "Hand €"
                      text @"amount" # projection euros
                      staticText " to the courier." ) # tapped ) # atCase @"Pay the courier" # toCase @"paidOut" identity ) # subChoice) # updated (match { refunded: informed applyRefund, paidOut: informed applyPayout, "Take a deposit": informed applyDeposit })
      ) # mvu openedTill
