module CashboxMDC3 (cashboxMDC3) where

import Prelude ((#), ($), Unit, identity)

import CashboxLogic (applyDeposit, applyPayout, applyRefund, balanceLine, courierFee, customerDeposit, openedTill, payoutLine, refundLine, standardRefund)
import Data.Profunctor.Row.RecordToVariant as RecordToVariant
import Data.Profunctor.Row.VariantToVariant as VariantToVariant
import Data.Variant (match)
import Effect (Effect)
import PUI (atCase, mvu, subChoice, toCase, updated, with)
import PUI.Web.HTML (shown, text)
import PUI.Web.MDC3 (body, bodyLarge, button, card, headlineSmall, confirmed)
import QualifiedDo.Category as Category

cashboxMDC3 :: Effect Unit
cashboxMDC3 =
  body $
    card $ ( Category.do
        ( headlineSmall $ text balanceLine ) # shown
        ( Category.do
            RecordToVariant.do
              button @"Refund a customer" { icon: "undo" } # with standardRefund
              button @"Pay the courier" { icon: "local_shipping" } # with courierFee
              button @"Take a deposit" { icon: "savings" } # with customerDeposit
            ( VariantToVariant.do
                ( confirmed { title: "Refund the customer?", confirm: "Refund" } $ bodyLarge $ text refundLine ) # atCase @"Refund a customer" # toCase @"refunded" identity
                ( confirmed { title: "Pay the courier?", confirm: "Pay" } $ bodyLarge $ text payoutLine ) # atCase @"Pay the courier" # toCase @"paidOut" identity ) # subChoice ) # updated (match { refunded: applyRefund, paidOut: applyPayout, "Take a deposit": applyDeposit })
    ) # mvu openedTill
