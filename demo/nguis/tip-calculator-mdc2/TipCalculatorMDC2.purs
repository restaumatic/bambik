module TipCalculatorMDC2 (tipCalculatorMDC2) where

import Prelude ((#), ($), Unit)

import Data.Profunctor.Row.RecordToRecord as RecordToRecord
import Effect (Effect)
import PUI (asField, completed, forField, mvu, projected, tapped)
import PUI.Web.HTML (body, staticText, text)
import PUI.Web.MDC2 (body2, card, elevation20, filledTextField, slider)
import QualifiedDo.Semigroupoid as Semigroupoid
import TipCalculatorLogic (dinnerBill, perPersonText, tipAmountText, totalText, whole)

tipCalculatorMDC2 :: Effect Unit
tipCalculatorMDC2 =
  body $
    elevation20 $
      card { caption: "Tip Calculator" } $ ( Semigroupoid.do
          filledTextField { floatingLabel: "Bill amount" } # asField @"amount" # completed
          slider { label: "Tip percentage" } # asField @"tipPercent" # completed
          body2 ( RecordToRecord.do
              staticText "Tip: "
              text # forField @"tipPercent" whole
              staticText "%" ) # tapped
          body2 ( RecordToRecord.do
              staticText "Split between: "
              text # forField @"people" whole
              staticText " people" ) # tapped
          slider { label: "Split between" } # asField @"people" # completed
          body2 ( RecordToRecord.do
              staticText "Tip amount: "
              text # projected tipAmountText ) # tapped
          body2 ( RecordToRecord.do
              staticText "Total: "
              text # projected totalText ) # tapped
          body2 ( RecordToRecord.do
              staticText "Per person: "
              text # projected perPersonText ) # tapped
      ) # mvu dinnerBill
