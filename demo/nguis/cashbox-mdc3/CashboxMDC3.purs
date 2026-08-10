module CashboxMDC3 (cashboxMDC3) where

import Prelude (identity, (#), ($), Unit)

import CashboxLogic (applyDeposit, applyPayout, applyRefund, courierFee, customerDeposit, euros, openedTill, standardRefund)
import Data.Profunctor.Row.RecordToRecord as RecordToRecord
import Data.Profunctor.Row.RecordToVariant as RecordToVariant
import Data.Profunctor.Row.VariantToVariant as VariantToVariant
import Data.Variant (match)
import Effect (Effect)
import PUI (asCase, subChoice, forField, informed, mvu, atCase, tapped, toCase, updated, with)
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
              text # forField @"value" @"balance" euros ) # tapped
          ( Semigroupoid.do
              RecordToVariant.do
                button { label: "Refund a customer", icon: "undo" } # with standardRefund # asCase @"clicked" @"refund"
                button { label: "Pay the courier", icon: "local_shipping" } # with courierFee # asCase @"clicked" @"payout"
                button { label: "Take a deposit", icon: "savings" } # with customerDeposit # asCase @"clicked" @"deposited"
              ( VariantToVariant.do
                  ( simpleDialog { title: "Refund the customer?", confirm: "Refund" } $ bodyLarge ( RecordToRecord.do
                      staticText "Hand €"
                      text # forField @"value" @"amount" euros
                      staticText " back to the customer." ) # tapped ) # atCase @"refund" # toCase @"refunded" identity
                  ( simpleDialog { title: "Pay the courier?", confirm: "Pay" } $ bodyLarge ( RecordToRecord.do
                      staticText "Hand €"
                      text # forField @"value" @"amount" euros
                      staticText " to the courier." ) # tapped ) # atCase @"payout" # toCase @"paidOut" identity ) # subChoice) # updated (match { refunded: informed applyRefund, paidOut: informed applyPayout, deposited: informed applyDeposit })
      ) # mvu openedTill
