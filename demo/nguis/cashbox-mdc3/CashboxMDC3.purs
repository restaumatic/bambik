module CashboxMDC3 (cashboxMDC3) where

import Prelude ((#), ($), Unit, identity)

import CashboxViewModel (balanceLine, openedTill, payCourier, payoutLine, refundLine, refundStandard, takeDeposit)
import Data.Profunctor.Row.RecordToVariant as RecordToVariant
import Data.Profunctor.Row.VariantToVariant as VariantToVariant
import Data.Profunctor.Row.VariantToRecord as VariantToRecord
import Effect (Effect)
import PUI (atCase, fold, looped, subChoice, toCase, with)
import PUI.Web (shown, text)
import PUI.Web.MDC3 (body, bodyLarge, button, headlineSmall, confirmed)
import QualifiedDo.Semigroupoid as Semigroupoid

cashboxMDC3 :: Effect Unit
cashboxMDC3 =
  body $
    ( Semigroupoid.do
      ( headlineSmall $ text balanceLine ) # shown
      RecordToVariant.do
        button @"Refund a customer" { icon: "undo" }
        button @"Pay the courier" { icon: "local_shipping" }
        button @"Take a deposit" { icon: "savings" }
      ( VariantToVariant.do
        ( confirmed @"Refund" @"Refund the customer?" $ bodyLarge $ text refundLine ) # atCase @"Refund a customer" # toCase @"refunded" identity
        ( confirmed @"Pay" @"Pay the courier?" $ bodyLarge $ text payoutLine ) # atCase @"Pay the courier" # toCase @"paidOut" identity ) # subChoice
      VariantToRecord.do
        fold @"refunded" refundStandard
        fold @"paidOut" payCourier
        fold @"Take a deposit" takeDeposit
    ) # looped @( balance :: Number ) # with openedTill
