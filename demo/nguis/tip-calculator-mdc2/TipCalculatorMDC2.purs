module TipCalculatorMDC2 (tipCalculatorMDC2) where

import Prelude ((#), ($), Unit)

import Effect (Effect)
import PUI (looped, suffixed, with)
import PUI.Pipeline as Pipeline
import PUI.Web (text)
import PUI.Web.HTML (rangeInput)
import PUI.Web.MDC2 (body, body2, filledTextField, slider)
import TipCalculatorViewModel (dinnerBill, perPersonLine, splitLine, tipAmountLine, tipLine, totalLine)

tipCalculatorMDC2 :: Effect Unit
tipCalculatorMDC2 =
  body $ Pipeline.do
    filledTextField @"Bill amount" {}
    slider @"Tip percentage" {}
    rangeInput @"Tip percentage"
    body2 (text tipLine)
    body2 (text splitLine)
    slider @"Split between" {} # suffixed ( Pipeline.do
      body2 (text tipAmountLine)
      body2 (text totalLine)
      body2 (text perPersonLine) )
  # looped
    @( "Bill amount" :: String
     , "Tip percentage" :: { current :: Number, min :: Number, max :: Number, step :: [ discrete :: Number, continuous :: {} ] }
     , "Split between" :: { current :: Number, min :: Number, max :: Number, step :: [ discrete :: Number, continuous :: {} ] }
     ) # with dinnerBill
