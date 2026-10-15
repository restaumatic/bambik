module Cashbox.MDC2 (cashbox) where

import Prelude ((#), ($), Unit)

import Cashbox (balanceLine, openedTill, payCourier, payoutLine, refundLine, refundStandard, takeDeposit)
import Data.Profunctor.Row.RecordToVariant as RecordToVariant
import Data.Profunctor.Row.VariantToRecord as VariantToRecord
import Effect (Effect)
import PUI (fold, looped, with)
import PUI.Web (text)
import PUI.Web.MDC2 (body, body1, button', headline6, confirmed)
import QualifiedDo.Semigroupoid as Semigroupoid

cashbox :: Effect Unit
cashbox =
  body $ Semigroupoid.do
    headline6 $ text balanceLine
    RecordToVariant.do
      button' @"Refund a customer" { icon: "undo" }
      button' @"Pay the courier" { icon: "local_shipping" }
      button' @"Take a deposit" { icon: "savings" }
    VariantToRecord.do
      confirmed @"Refund a customer" "Refund" refundStandard $ body1 $ text refundLine
      confirmed @"Pay the courier" "Pay" payCourier $ body1 $ text payoutLine
      fold @"Take a deposit" takeDeposit
  # looped # with openedTill
