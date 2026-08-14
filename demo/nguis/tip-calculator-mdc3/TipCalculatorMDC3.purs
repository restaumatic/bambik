module TipCalculatorMDC3 (tipCalculatorMDC3) where

import Prelude ((#), ($), Unit)

import Data.Profunctor.Row.RecordToRecord as RecordToRecord
import Effect (Effect)
import PUI (completed, projection, mvu, projected, tapped)
import PUI.Web.HTML (body, staticText, text)
import PUI.Web.MDC3 (bodyMedium, card, elevation5, filledTextField, slider)
import QualifiedDo.Semigroupoid as Semigroupoid
import TipCalculatorLogic (dinnerBill, perPersonText, tipAmountText, totalText, whole)

tipCalculatorMDC3 :: Effect Unit
tipCalculatorMDC3 =
  body $
    elevation5 $
      card { caption: "Tip Calculator" } $ ( Semigroupoid.do
          filledTextField @"amount" { floatingLabel: "Bill amount" } # completed
          slider @"tipPercent" { label: "Tip percentage" } # completed
          bodyMedium ( RecordToRecord.do
              staticText "Tip: "
              text @"tipPercent" # projection whole
              staticText "%" ) # tapped
          bodyMedium ( RecordToRecord.do
              staticText "Split between: "
              text @"people" # projection whole
              staticText " people" ) # tapped
          slider @"people" { label: "Split between" } # completed
          bodyMedium ( RecordToRecord.do
              staticText "Tip amount: "
              text @"tipAmount" # projected tipAmountText ) # tapped
          bodyMedium ( RecordToRecord.do
              staticText "Total: "
              text @"total" # projected totalText ) # tapped
          bodyMedium ( RecordToRecord.do
              staticText "Per person: "
              text @"perPerson" # projected perPersonText ) # tapped
      ) # mvu dinnerBill
