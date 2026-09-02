module CashboxMDC3 (cashboxMDC3) where

import Prelude ((#), ($), Unit)

import CashboxLogic (applyDeposit, applyPayout, applyRefund, courierFee, customerDeposit, openedTill, payoutSum, presentCashbox, refundSum, standardRefund)
import Data.Profunctor.Row.RecordToVariant as RecordToVariant
import Data.Profunctor.Row.VariantToVariant as VariantToVariant
import Data.Variant (match)
import Effect (Effect)
import PUI (atCase, mvu, settled, subChoice, toCase, updated, with)
import PUI.Web.HTML (shown, body, text)
import PUI.Web.MDC3 (bodyLarge, button, card, elevation5, headlineSmall, confirmed)
import QualifiedDo.Category as Category

cashboxMDC3 :: Effect Unit
cashboxMDC3 =
  body $
    elevation5 $
      card $ ( Category.do
          ( headlineSmall $ text @"balanceLine" ) # shown
          ( Category.do
              RecordToVariant.do
                button @"Refund a customer" { icon: "undo" } # with standardRefund
                button @"Pay the courier" { icon: "local_shipping" } # with courierFee
                button @"Take a deposit" { icon: "savings" } # with customerDeposit
              ( VariantToVariant.do
                  ( confirmed { title: "Refund the customer?", confirm: "Refund" } $ bodyLarge $ text @"refundLine" ) # atCase @"Refund a customer" # toCase @"Refunded the customer" refundSum
                  ( confirmed { title: "Pay the courier?", confirm: "Pay" } $ bodyLarge $ text @"payoutLine" ) # atCase @"Pay the courier" # toCase @"Paid the courier" payoutSum ) # subChoice ) # updated (match { "Refunded the customer": applyRefund, "Paid the courier": applyPayout, "Take a deposit": applyDeposit })
      ) # settled presentCashbox # mvu openedTill
