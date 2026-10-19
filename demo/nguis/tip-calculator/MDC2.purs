module TipCalculator.MDC2 (tipCalculator) where

import Prelude ((#), ($), Unit)

import Effect (Effect)
import PUI (looped, with)
import PUI.Web (text)
import PUI.Web.HTML (rangeInput)
import PUI.Web.MDC2 (body, body2, filledTextField, slider)
import QualifiedDo.Semigroupoid as Semigroupoid
import TipCalculator (dinnerBill, perPersonLine, splitLine, tipAmountLine, tipLine, totalLine)

tipCalculator :: Effect Unit
tipCalculator =
  body $ Semigroupoid.do
    filledTextField @"Bill amount"
    slider @"Tip percentage"
    rangeInput @"Tip percentage"
    body2 $ text tipLine
    slider @"Split between"
    body2 $ text splitLine
    body2 $ text tipAmountLine
    body2 $ text totalLine
    body2 $ text perPersonLine
  # looped # with dinnerBill
