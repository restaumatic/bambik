module TipCalculatorMDC2 (tipCalculatorMDC2) where

import Prelude ((#), ($), Unit)

import Data.Profunctor.Row.RecordToRecord as RecordToRecord
import Effect (Effect)
import PUI (mvu, settled)
import PUI.Web.HTML (rangeInput, shown, body, staticText, text)
import PUI.Web.MDC2 (body2, card, elevation20, filledTextField, slider)
import QualifiedDo.Category as Category
import TipCalculatorLogic (dinnerBill, presentTips)

tipCalculatorMDC2 :: Effect Unit
tipCalculatorMDC2 =
  body $
    elevation20 $
      card $ ( Category.do
          filledTextField @"Bill amount" {}
          slider @"Tip percentage" {}
          rangeInput @"Tip percentage"
          ( body2 $ RecordToRecord.do
              staticText "Tip: "
              text @"tipText"
              staticText "%" ) # shown
          ( body2 $ RecordToRecord.do
              staticText "Split between: "
              text @"splitText"
              staticText " people" ) # shown
          slider @"Split between" {}
          ( body2 $ RecordToRecord.do
              staticText "Tip amount: "
              text @"tipAmountText" ) # shown
          ( body2 $ RecordToRecord.do
              staticText "Total: "
              text @"totalText" ) # shown
          ( body2 $ RecordToRecord.do
              staticText "Per person: "
              text @"perPersonText" ) # shown
      ) # settled presentTips # mvu dinnerBill
