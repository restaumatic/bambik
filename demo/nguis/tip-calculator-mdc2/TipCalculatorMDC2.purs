module TipCalculatorMDC2 (tipCalculatorMDC2) where

import Prelude ((#), ($), Unit)

import Data.Profunctor.Row.RecordToRecord as RecordToRecord
import Effect (Effect)
import PUI (joint, projection, mvu, projected)
import PUI.Web.HTML (rangeInput, shown, body, staticText, text)
import PUI.Web.MDC2 (body2, card, elevation20, filledTextField, slider)
import QualifiedDo.Semigroupoid as Pipeline
import TipCalculatorLogic (dinnerBill, perPersonText, tipAmountText, totalText, whole)

tipCalculatorMDC2 :: Effect Unit
tipCalculatorMDC2 =
  body $
    elevation20 $
      card $ ( Pipeline.do
          ( filledTextField @"Bill amount" {}
              `joint` slider @"Tip percentage" {}
              `joint` rangeInput @"Tip percentage" )
          ( body2 $ RecordToRecord.do
              staticText "Tip: "
              text @"Tip percentage" # projection whole
              staticText "%" ) # shown
          ( body2 $ RecordToRecord.do
              staticText "Split between: "
              text @"Split between" # projection whole
              staticText " people" ) # shown
          slider @"Split between" {}
          ( body2 $ RecordToRecord.do
              staticText "Tip amount: "
              text @"tipAmount" # projected tipAmountText ) # shown
          ( body2 $ RecordToRecord.do
              staticText "Total: "
              text @"total" # projected totalText ) # shown
          ( body2 $ RecordToRecord.do
              staticText "Per person: "
              text @"perPerson" # projected perPersonText ) # shown
      ) # mvu dinnerBill
