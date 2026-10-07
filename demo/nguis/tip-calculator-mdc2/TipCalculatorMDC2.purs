module TipCalculatorMDC2 (tipCalculatorMDC2) where

import Prelude ((#), ($), Unit)

import Data.Profunctor.Row.RecordUpdate as RecordUpdate
import Effect (Effect)
import PUI (looped, with)
import PUI.Web (shown, text)
import PUI.Web.HTML (rangeInput)
import PUI.Web.MDC2 (body, body2, filledTextField, slider)
import TipCalculatorViewModel (dinnerBill, perPersonLine, splitLine, tipAmountLine, tipLine, totalLine)

tipCalculatorMDC2 :: Effect Unit
tipCalculatorMDC2 =
  body $
    RecordUpdate.do
      filledTextField @"Bill amount" {}
      slider @"Tip percentage" {}
      rangeInput @"Tip percentage"
      body2 (text tipLine)
      body2 (text splitLine)
      slider @"Split between" {}
      body2 (text tipAmountLine)
      body2 (text totalLine)
      body2 (text perPersonLine) # shown
    # looped
      @( "Bill amount" :: String
       , "Tip percentage" :: { current :: Number, min :: Number, max :: Number, step :: [ discrete :: Number, continuous :: {} ] }
       , "Split between" :: { current :: Number, min :: Number, max :: Number, step :: [ discrete :: Number, continuous :: {} ] }
       ) # with dinnerBill
