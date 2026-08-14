module CashboxMDC2 (cashboxMDC2) where

import Prelude (identity, (#), ($), Unit)

import CashboxLogic (applyDeposit, applyPayout, applyRefund, courierFee, customerDeposit, euros, openedTill, standardRefund)
import Data.Profunctor.Row.RecordToRecord as RecordToRecord
import Data.Profunctor.Row.RecordToVariant as RecordToVariant
import Data.Profunctor.Row.VariantToVariant as VariantToVariant
import Data.Variant (match)
import Effect (Effect)
import PUI (atCase, projection, informed, mvu, subChoice, tapped, toCase, updated, with)
import PUI.Web.HTML (body, staticText, text)
import PUI.Web.MDC2 (body1, button, card, elevation20, headline6, simpleDialog)
import QualifiedDo.Semigroupoid as Semigroupoid

cashboxMDC2 :: Effect Unit
cashboxMDC2 =
  body $
    elevation20 $
      card { caption: "Cashbox" } $ ( Semigroupoid.do
          headline6 ( RecordToRecord.do
              staticText "Till balance: €"
              text @"balance" # projection euros ) # tapped
          ( Semigroupoid.do
              RecordToVariant.do
                button @"Refund a customer" { icon: "undo" } # with standardRefund
                button @"Pay the courier" { icon: "local_shipping" } # with courierFee
                button @"Take a deposit" { icon: "savings" } # with customerDeposit
              ( VariantToVariant.do
                  ( simpleDialog { title: "Refund the customer?", confirm: "Refund" } $ body1 ( RecordToRecord.do
                      staticText "Hand €"
                      text @"amount" # projection euros
                      staticText " back to the customer." ) # tapped ) # atCase @"Refund a customer" # toCase @"refunded" identity
                  ( simpleDialog { title: "Pay the courier?", confirm: "Pay" } $ body1 ( RecordToRecord.do
                      staticText "Hand €"
                      text @"amount" # projection euros
                      staticText " to the courier." ) # tapped ) # atCase @"Pay the courier" # toCase @"paidOut" identity ) # subChoice) # updated (match { refunded: informed applyRefund, paidOut: informed applyPayout, "Take a deposit": informed applyDeposit })
      ) # mvu openedTill
