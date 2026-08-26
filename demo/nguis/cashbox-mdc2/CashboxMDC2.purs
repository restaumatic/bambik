module CashboxMDC2 (cashboxMDC2) where

import Prelude (identity, (#), ($), Unit)

import CashboxLogic (balanceLine, courierLine, refundLine, applyDeposit, applyPayout, applyRefund, courierFee, customerDeposit, openedTill, standardRefund)
import Data.Profunctor.Row.RecordToVariant as RecordToVariant
import Data.Profunctor.Row.VariantToVariant as VariantToVariant
import Data.Variant (match)
import Effect (Effect)
import PUI (atCase, mvu, subChoice, toCase, updated, with)
import PUI.Web.HTML (told, body)
import PUI.Web.MDC2 (body1, button, card, elevation20, headline6, confirmed)
import QualifiedDo.Semigroupoid as Pipeline

cashboxMDC2 :: Effect Unit
cashboxMDC2 =
  body $
    elevation20 $
      card $ ( Pipeline.do
          headline6 (told balanceLine)
          ( Pipeline.do
              RecordToVariant.do
                button @"Refund a customer" { icon: "undo" } # with standardRefund
                button @"Pay the courier" { icon: "local_shipping" } # with courierFee
                button @"Take a deposit" { icon: "savings" } # with customerDeposit
              ( VariantToVariant.do
                  ( body1 (told refundLine) # confirmed { title: "Refund the customer?", confirm: "Refund" } ) # atCase @"Refund a customer" # toCase @"refunded" identity
                  ( body1 (told courierLine) # confirmed { title: "Pay the courier?", confirm: "Pay" } ) # atCase @"Pay the courier" # toCase @"paidOut" identity ) # subChoice ) # updated (match { refunded: applyRefund, paidOut: applyPayout, "Take a deposit": applyDeposit })
      ) # mvu openedTill
