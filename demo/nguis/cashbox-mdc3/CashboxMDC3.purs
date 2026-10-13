module CashboxMDC3 (cashboxMDC3) where

import Prelude ((#), ($), Unit, identity)

import CashboxViewModel (balanceLine, openedTill, payCourier, payoutLine, refundLine, refundStandard, takeDeposit)
import Data.Profunctor.Row.RecordToVariant as RecordToVariant
import Data.Profunctor.Row.VariantToRecord as VariantToRecord
import Effect (Effect)
import PUI (atCase, fold, looped, prefixed, toCase, unshown, with)
import PUI.Web (text)
import PUI.Web.MDC3 (body, bodyLarge, buttonWith, headlineSmall, confirmed)
import QualifiedDo.Semigroupoid as Semigroupoid

cashboxMDC3 :: Effect Unit
cashboxMDC3 =
  body $ Semigroupoid.do
    prefixed (headlineSmall $ text balanceLine) $ RecordToVariant.do
      buttonWith @"Refund a customer" { icon: "undo" }
      buttonWith @"Pay the courier" { icon: "local_shipping" }
      buttonWith @"Take a deposit" { icon: "savings" }
    VariantToRecord.do
      ( Semigroupoid.do
        ( confirmed "Refund" "Refund the customer?" $ bodyLarge $ text refundLine ) # toCase @"Customer refunded" identity
        unshown @"Customer refunded" # fold refundStandard ) # atCase @"Refund a customer"
      ( Semigroupoid.do
        ( confirmed "Pay" "Pay the courier?" $ bodyLarge $ text payoutLine ) # toCase @"Courier paid out" identity
        unshown @"Courier paid out" # fold payCourier ) # atCase @"Pay the courier"
      unshown @"Take a deposit" # fold takeDeposit
  # looped @( balance :: Number ) # with openedTill
