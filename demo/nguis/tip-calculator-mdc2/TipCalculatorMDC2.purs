module TipCalculatorMDC2 (tipCalculatorMDC2) where

import Prelude ((#), ($), Unit)

import Effect (Effect)
import PUI (mvu)
import PUI.Web (shown, text)
import PUI.Web.HTML (rangeInput)
import PUI.Web.MDC2 (body, body2, filledTextField, slider)
import QualifiedDo.Semigroupoid as Semigroupoid
import TipCalculatorViewModel (dinnerBill, perPersonLine, splitLine, tipAmountLine, tipLine, totalLine)

tipCalculatorMDC2 :: Effect Unit
tipCalculatorMDC2 =
  body $
    ( Semigroupoid.do
      filledTextField @"Bill amount" {}
      slider @"Tip percentage" {}
      rangeInput @"Tip percentage"
      body2 (text tipLine) # shown
      body2 (text splitLine) # shown
      slider @"Split between" {}
      body2 (text tipAmountLine) # shown
      body2 (text totalLine) # shown
      body2 (text perPersonLine) # shown
    ) # mvu
      @( "Bill amount" :: String
       , "Tip percentage" :: { current :: Number, min :: Number, max :: Number, step :: [ discrete :: Number, continuous :: {} ] }
       , "Split between" :: { current :: Number, min :: Number, max :: Number, step :: [ discrete :: Number, continuous :: {} ] }
       )
      dinnerBill
