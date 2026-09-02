module TipCalculatorMDC2 (tipCalculatorMDC2) where

import Prelude ((#), ($), Unit)

import Effect (Effect)
import PUI (mvu, settled)
import PUI.Web.HTML (rangeInput, shown, body, text)
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
          body2 (text @"tipLine") # shown
          body2 (text @"splitLine") # shown
          slider @"Split between" {}
          body2 (text @"tipAmountLine") # shown
          body2 (text @"totalLine") # shown
          body2 (text @"perPersonLine") # shown
      ) # settled presentTips # mvu dinnerBill
