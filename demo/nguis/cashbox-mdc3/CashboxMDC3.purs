module CashboxMDC3 (cashboxMDC3) where

import Prelude (identity, (#), ($), Unit)

import CashboxLogic (applyDeposit, applyPayout, applyRefund, courierFee, customerDeposit, euros, openedTill, standardRefund)
import Data.Profunctor.Row.RecordToRecord as RecordToRecord
import Data.Profunctor.Row.RecordToVariant as RecordToVariant
import Data.Profunctor.Row.VariantToVariant as VariantToVariant
import Data.Variant (match)
import Effect (Effect)
import PUI (atCase, projection, mvu, subChoice, toCase, updated, with)
import PUI.Web.HTML (shown, body, staticText, text)
import PUI.Web.MDC3 (bodyLarge, button, card, elevation5, headlineSmall, confirmed)
import QualifiedDo.Category as Category

cashboxMDC3 :: Effect Unit
cashboxMDC3 =
  body $
    elevation5 $
      card $ ( Category.do
          ( headlineSmall $ RecordToRecord.do
              staticText "Till balance: €"
              text @"balance" # projection euros ) # shown
          ( Category.do
              RecordToVariant.do
                button @"Refund a customer" { icon: "undo" } # with standardRefund
                button @"Pay the courier" { icon: "local_shipping" } # with courierFee
                button @"Take a deposit" { icon: "savings" } # with customerDeposit
              ( VariantToVariant.do
                  ( confirmed { title: "Refund the customer?", confirm: "Refund" } $ bodyLarge $ RecordToRecord.do
                      staticText "Hand €"
                      text @"amount" # projection euros
                      staticText " back to the customer." ) # atCase @"Refund a customer" # toCase @"Refunded the customer" identity
                  ( confirmed { title: "Pay the courier?", confirm: "Pay" } $ bodyLarge $ RecordToRecord.do
                      staticText "Hand €"
                      text @"amount" # projection euros
                      staticText " to the courier." ) # atCase @"Pay the courier" # toCase @"Paid the courier" identity ) # subChoice ) # updated (match { "Refunded the customer": applyRefund, "Paid the courier": applyPayout, "Take a deposit": applyDeposit })
      ) # mvu openedTill
