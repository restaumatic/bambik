module CashboxMDC2 (cashboxMDC2) where

import Prelude ((#), ($), Unit, identity)

import CashboxViewModel (balanceLine, openedTill, payCourier, payoutLine, refundLine, refundStandard, takeDeposit)
import Data.Profunctor.Row.RecordToVariant as RecordToVariant
import Data.Profunctor.Row.VariantToVariant as VariantToVariant
import Data.Profunctor.Row.VariantToRecord as VariantToRecord
import Effect (Effect)
import PUI (atCase, fold, looped, prefixed, subChoice, toCase, unshown, with)
import PUI.Web (text)
import PUI.Web.MDC2 (body, body1, buttonWith, headline6, confirmed)
import QualifiedDo.Semigroupoid as Semigroupoid

cashboxMDC2 :: Effect Unit
cashboxMDC2 =
  body $ Semigroupoid.do
    prefixed (headline6 $ text balanceLine) $ RecordToVariant.do
      buttonWith @"Refund a customer" { icon: "undo" }
      buttonWith @"Pay the courier" { icon: "local_shipping" }
      buttonWith @"Take a deposit" { icon: "savings" }
    ( VariantToVariant.do
      ( confirmed "Refund" "Refund the customer?" $ body1 $ text refundLine ) # atCase @"Refund a customer" # toCase @"Customer refunded" identity
      ( confirmed "Pay" "Pay the courier?" $ body1 $ text payoutLine ) # atCase @"Pay the courier" # toCase @"Courier paid out" identity ) # subChoice
    VariantToRecord.do
      unshown @"Customer refunded" # fold refundStandard
      unshown @"Courier paid out" # fold payCourier
      unshown @"Take a deposit" # fold takeDeposit
  # looped @( balance :: Number ) # with openedTill
