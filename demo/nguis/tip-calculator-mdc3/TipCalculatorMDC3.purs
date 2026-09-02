module TipCalculatorMDC3 (tipCalculatorMDC3) where

import Prelude ((#), ($), Unit)

import Effect (Effect)
import PUI (mvu, settled)
import PUI.Web.HTML (shown, body, text)
import PUI.Web.MDC3 (bodyMedium, card, elevation5, filledTextField, slider)
import QualifiedDo.Category as Category
import TipCalculatorLogic (dinnerBill, presentTips)

tipCalculatorMDC3 :: Effect Unit
tipCalculatorMDC3 =
  body $
    elevation5 $
      card $ ( Category.do
          filledTextField @"Bill amount" {}
          slider @"Tip percentage" {}
          bodyMedium (text @"tipLine") # shown
          bodyMedium (text @"splitLine") # shown
          slider @"Split between" {}
          bodyMedium (text @"tipAmountLine") # shown
          bodyMedium (text @"totalLine") # shown
          bodyMedium (text @"perPersonLine") # shown
      ) # settled presentTips # mvu dinnerBill
