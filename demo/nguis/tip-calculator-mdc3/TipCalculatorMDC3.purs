module TipCalculatorMDC3 (tipCalculatorMDC3) where

import Prelude ((#), ($), Unit)

import Data.Profunctor.Row.RecordToRecord as RecordToRecord
import Effect (Effect)
import PUI (asField, completed, forField, mvu, projected, tapped)
import PUI.Web.HTML (body, staticText, text)
import PUI.Web.MDC3 (bodyMedium, card, elevation5, filledTextField, slider)
import QualifiedDo.Semigroupoid as Semigroupoid
import TipCalculatorLogic (dinnerBill, perPersonText, tipAmountText, totalText, whole)

tipCalculatorMDC3 :: Effect Unit
tipCalculatorMDC3 =
  body $
    elevation5 $
      card { caption: "Tip Calculator" } $ ( Semigroupoid.do
          filledTextField { floatingLabel: "Bill amount" } # asField @"amount" # completed
          slider { label: "Tip percentage" } # asField @"tipPercent" # completed
          bodyMedium ( RecordToRecord.do
              staticText "Tip: "
              text # forField @"tipPercent" whole
              staticText "%" ) # tapped
          bodyMedium ( RecordToRecord.do
              staticText "Split between: "
              text # forField @"people" whole
              staticText " people" ) # tapped
          slider { label: "Split between" } # asField @"people" # completed
          bodyMedium ( RecordToRecord.do
              staticText "Tip amount: "
              text # projected tipAmountText ) # tapped
          bodyMedium ( RecordToRecord.do
              staticText "Total: "
              text # projected totalText ) # tapped
          bodyMedium ( RecordToRecord.do
              staticText "Per person: "
              text # projected perPersonText ) # tapped
      ) # mvu dinnerBill
