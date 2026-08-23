module CashboxMDC2 (cashboxMDC2) where

import Prelude (identity, (#), ($), Unit)

import CashboxLogic (balanceText, courierText, refundText, applyDeposit, applyPayout, applyRefund, courierFee, customerDeposit, openedTill, standardRefund)
import Data.Profunctor.Row.RecordToVariant as RecordToVariant
import Data.Profunctor.Row.VariantToVariant as VariantToVariant
import Data.Variant (match)
import Effect (Effect)
import PUI (atCase, informed, mvu, subChoice, toCase, updated, with)
import PUI.Web.HTML (shown, body)
import PUI.Web.MDC2 (body1, button, card, elevation20, headline6, confirmed)
import QualifiedDo.Semigroupoid as Semigroupoid

cashboxMDC2 :: Effect Unit
cashboxMDC2 =
  body $
    elevation20 $
      card $ ( Semigroupoid.do
          headline6 (shown @"balance" balanceText)
          ( Semigroupoid.do
              RecordToVariant.do
                button @"Refund a customer" { icon: "undo" } # with standardRefund
                button @"Pay the courier" { icon: "local_shipping" } # with courierFee
                button @"Take a deposit" { icon: "savings" } # with customerDeposit
              ( VariantToVariant.do
                  ( confirmed { title: "Refund the customer?", confirm: "Refund" } $ body1 (shown @"amount" refundText) ) # atCase @"Refund a customer" # toCase @"refunded" identity
                  ( confirmed { title: "Pay the courier?", confirm: "Pay" } $ body1 (shown @"amount" courierText) ) # atCase @"Pay the courier" # toCase @"paidOut" identity ) # subChoice ) # updated (match { refunded: informed applyRefund, paidOut: informed applyPayout, "Take a deposit": informed applyDeposit })
      ) # mvu openedTill
