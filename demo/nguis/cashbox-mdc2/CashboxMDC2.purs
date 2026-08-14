module CashboxMDC2 (cashboxMDC2) where

import Prelude ((>>>), identity, (#), ($), Unit)

import CashboxLogic (applyDeposit, applyPayout, applyRefund, courierFee, customerDeposit, euros, openedTill, standardRefund)
import Data.Profunctor.Row.RecordToRecord as RecordToRecord
import Data.Profunctor.Row.RecordToVariant as RecordToVariant
import Data.Profunctor.Row.VariantToVariant as VariantToVariant
import Data.Variant (match)
import Effect (Effect)
import PUI (announce, asCase, atCase, forField, informed, mvu, subChoice, tapped, toCase, updated)
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
              text @"value" # forField @"balance" euros ) # tapped
          ( Semigroupoid.do
              RecordToVariant.do
                announce standardRefund >>> button { label: "Refund a customer", icon: "undo" } # asCase @"clicked" @"refund"
                announce courierFee >>> button { label: "Pay the courier", icon: "local_shipping" } # asCase @"clicked" @"payout"
                announce customerDeposit >>> button { label: "Take a deposit", icon: "savings" } # asCase @"clicked" @"deposited"
              ( VariantToVariant.do
                  ( simpleDialog { title: "Refund the customer?", confirm: "Refund" } $ body1 ( RecordToRecord.do
                      staticText "Hand €"
                      text @"value" # forField @"amount" euros
                      staticText " back to the customer." ) # tapped ) # atCase @"refund" # toCase @"refunded" identity
                  ( simpleDialog { title: "Pay the courier?", confirm: "Pay" } $ body1 ( RecordToRecord.do
                      staticText "Hand €"
                      text @"value" # forField @"amount" euros
                      staticText " to the courier." ) # tapped ) # atCase @"payout" # toCase @"paidOut" identity ) # subChoice) # updated (match { refunded: informed applyRefund, paidOut: informed applyPayout, deposited: informed applyDeposit })
      ) # mvu openedTill
