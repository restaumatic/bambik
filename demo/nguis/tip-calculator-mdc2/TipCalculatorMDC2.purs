module TipCalculatorMDC2 (tipCalculatorMDC2) where

import Prelude ((#), ($), Unit)

import Data.Profunctor.Row.RecordToRecord as RecordToRecord
import Effect (Effect)
import PUI (completed, forField, mvu, projected, tapped)
import PUI.Web.HTML (body, staticText, text)
import PUI.Web.MDC2 (body2, card, elevation20, filledTextField, slider)
import QualifiedDo.Semigroupoid as Semigroupoid
import TipCalculatorLogic (dinnerBill, perPersonText, tipAmountText, totalText, whole)

tipCalculatorMDC2 :: Effect Unit
tipCalculatorMDC2 =
  body $
    elevation20 $
      card { caption: "Tip Calculator" } $ ( Semigroupoid.do
          filledTextField @"amount" { floatingLabel: "Bill amount" } # completed
          slider @"tipPercent" { label: "Tip percentage" } # completed
          body2 ( RecordToRecord.do
              staticText "Tip: "
              text @"value" # forField @"tipPercent" whole
              staticText "%" ) # tapped
          body2 ( RecordToRecord.do
              staticText "Split between: "
              text @"value" # forField @"people" whole
              staticText " people" ) # tapped
          slider @"people" { label: "Split between" } # completed
          body2 ( RecordToRecord.do
              staticText "Tip amount: "
              text @"value" # projected @"value" tipAmountText ) # tapped
          body2 ( RecordToRecord.do
              staticText "Total: "
              text @"value" # projected @"value" totalText ) # tapped
          body2 ( RecordToRecord.do
              staticText "Per person: "
              text @"value" # projected @"value" perPersonText ) # tapped
      ) # mvu dinnerBill
