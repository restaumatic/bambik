module Cashbox.MDC2 (cashbox) where

import Prelude ((#), ($), Unit)

import Cashbox (balanceLine, openedTill, payCourier, payoutLine, refundLine, refundStandard, takeDeposit)
import Data.Profunctor.Row.RecordToVariant as RecordToVariant
import Data.Profunctor.Row.VariantToRecord as VariantToRecord
import Effect (Effect)
import PUI (fold, looped, with)
import PUI.Web (text)
import PUI.Web.MDC2 (body, body1, button', confirmed, headline6)
import QualifiedDo.Semigroupoid as Semigroupoid

cashbox :: Effect Unit
cashbox =
  body $ Semigroupoid.do
    headline6 $ text balanceLine
    RecordToVariant.do
      button' @"Refund a customer" { icon: "undo" }
      button' @"Pay the courier" { icon: "local_shipping" }
      button' @"Take a deposit" { icon: "savings" }
    confirmed @"Refund a customer" $ body1 $ text refundLine
    confirmed @"Pay the courier" $ body1 $ text payoutLine
    VariantToRecord.do
      fold @"Refund a customer" refundStandard
      fold @"Pay the courier" payCourier
      fold @"Take a deposit" takeDeposit
  # looped # with openedTill
