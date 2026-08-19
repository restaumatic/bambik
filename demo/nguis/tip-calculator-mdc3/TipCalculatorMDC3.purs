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
      card $ ( Semigroupoid.do
          filledTextField @"Bill amount" {} # completed
          slider @"Tip percentage" {} # completed
          bodyMedium ( RecordToRecord.do
              staticText "Tip: "
              text @"Tip percentage" # projection whole
              staticText "%" ) # tapped
          bodyMedium ( RecordToRecord.do
              staticText "Split between: "
              text @"Split between" # projection whole
              staticText " people" ) # tapped
          slider @"Split between" {} # completed
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
