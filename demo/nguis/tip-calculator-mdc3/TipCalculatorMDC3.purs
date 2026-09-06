module TipCalculatorMDC3 (tipCalculatorMDC3) where

import Prelude ((#), ($), Unit)

import Effect (Effect)
import PUI (mvu)
import PUI.Web.HTML (rangeInput, shown, text)
import PUI.Web.MDC3 (body, bodyMedium, card, elevation5, filledTextField, slider)
import QualifiedDo.Category as Category
import TipCalculatorLogic (dinnerBill, perPersonLine, splitLine, tipAmountLine, tipLine, totalLine)

tipCalculatorMDC3 :: Effect Unit
tipCalculatorMDC3 =
  body $
    elevation5 $
      card $ ( Category.do
          filledTextField @"Bill amount" {}
          slider @"Tip percentage" {}
          rangeInput @"Tip percentage"
          bodyMedium (text tipLine) # shown
          bodyMedium (text splitLine) # shown
          slider @"Split between" {}
          bodyMedium (text tipAmountLine) # shown
          bodyMedium (text totalLine) # shown
          bodyMedium (text perPersonLine) # shown
      ) # mvu dinnerBill
