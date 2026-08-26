module TipCalculatorMDC3 (tipCalculatorMDC3) where

import Prelude ((#), ($), Unit)

import Data.Profunctor.Row.RecordToRecord as RecordToRecord
import Effect (Effect)
import PUI (projection, mvu, projected)
import PUI.Web.HTML (shown, body, staticText, text)
import PUI.Web.MDC3 (bodyMedium, card, elevation5, filledTextField, slider)
import QualifiedDo.Category as Category
import TipCalculatorLogic (dinnerBill, perPersonText, tipAmountText, totalText, whole)

tipCalculatorMDC3 :: Effect Unit
tipCalculatorMDC3 =
  body $
    elevation5 $
      card $ ( Category.do
          filledTextField @"Bill amount" {}
          slider @"Tip percentage" {}
          ( bodyMedium $ RecordToRecord.do
              staticText "Tip: "
              text @"Tip percentage" # projection whole
              staticText "%" ) # shown
          ( bodyMedium $ RecordToRecord.do
              staticText "Split between: "
              text @"Split between" # projection whole
              staticText " people" ) # shown
          slider @"Split between" {}
          ( bodyMedium $ RecordToRecord.do
              staticText "Tip amount: "
              text @"tipAmount" # projected tipAmountText ) # shown
          ( bodyMedium $ RecordToRecord.do
              staticText "Total: "
              text @"total" # projected totalText ) # shown
          ( bodyMedium $ RecordToRecord.do
              staticText "Per person: "
              text @"perPerson" # projected perPersonText ) # shown
      ) # mvu dinnerBill
