module Cashbox.MDC3 (cashbox) where

import Prelude ((#), ($), Unit)

import Cashbox (balanceLine, openedTill, payCourier, payoutLine, refundLine, refundStandard, takeDeposit)
import Data.Profunctor.Row.RecordToVariant as RecordToVariant
import Data.Profunctor.Row.VariantToRecord as VariantToRecord
import Effect (Effect)
import PUI (fold, looped, with)
import PUI.Web (text)
import PUI.Web.MDC3 (body, bodyLarge, button', headlineSmall, confirmed)
import QualifiedDo.Semigroupoid as Semigroupoid

cashbox :: Effect Unit
cashbox =
  body $ Semigroupoid.do
    headlineSmall $ text balanceLine
    RecordToVariant.do
      button' @"Refund a customer" { icon: "undo" }
      button' @"Pay the courier" { icon: "local_shipping" }
      button' @"Take a deposit" { icon: "savings" }
    confirmed @"Refund a customer" $ bodyLarge $ text refundLine
    confirmed @"Pay the courier" $ bodyLarge $ text payoutLine
    VariantToRecord.do
      fold @"Refund a customer" refundStandard
      fold @"Pay the courier" payCourier
      fold @"Take a deposit" takeDeposit
  # looped # with openedTill
