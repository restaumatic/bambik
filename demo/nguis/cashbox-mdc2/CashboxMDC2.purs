module CashboxMDC2 (cashboxMDC2) where

import Prelude ((#), ($), Unit, identity)

import CashboxViewModel (balanceLine, openedTill, payCourier, payoutLine, refundLine, refundStandard, takeDeposit)
import Data.Profunctor.Row.RecordToVariant as RecordToVariant
import Data.Profunctor.Row.VariantToVariant as VariantToVariant
import Data.Profunctor.Row.VariantToRecord as VariantToRecord
import Effect (Effect)
import PUI (atCase, fold, mvu, subChoice, toCase)
import PUI.Web (shown, text)
import PUI.Web.MDC2 (body, body1, button, headline6, confirmed)
import QualifiedDo.Semigroupoid as Semigroupoid

cashboxMDC2 :: Effect Unit
cashboxMDC2 =
  body $
    ( Semigroupoid.do
      ( headline6 $ text balanceLine ) # shown
      RecordToVariant.do
        button @"Refund a customer" { icon: "undo" }
        button @"Pay the courier" { icon: "local_shipping" }
        button @"Take a deposit" { icon: "savings" }
      ( VariantToVariant.do
        ( confirmed @"Refund" @"Refund the customer?" $ body1 $ text refundLine ) # atCase @"Refund a customer" # toCase @"refunded" identity
        ( confirmed @"Pay" @"Pay the courier?" $ body1 $ text payoutLine ) # atCase @"Pay the courier" # toCase @"paidOut" identity ) # subChoice
      VariantToRecord.do
        fold @"refunded" refundStandard
        fold @"paidOut" payCourier
        fold @"Take a deposit" takeDeposit
    ) # mvu @( balance :: Number ) openedTill
