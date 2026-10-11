module TipCalculatorMDC3 (tipCalculatorMDC3) where

import Prelude ((#), ($), Unit)

import Effect (Effect)
import PUI (looped, suffixed, with)
import PUI.Pipeline as Pipeline
import PUI.Web (text)
import PUI.Web.HTML (rangeInput)
import PUI.Web.MDC3 (body, bodyMedium, filledTextField, slider)
import TipCalculatorViewModel (dinnerBill, perPersonLine, splitLine, tipAmountLine, tipLine, totalLine)

tipCalculatorMDC3 :: Effect Unit
tipCalculatorMDC3 =
  body $ Pipeline.do
    filledTextField @"Bill amount" {}
    slider @"Tip percentage" {}
    rangeInput @"Tip percentage"
    bodyMedium (text tipLine)
    bodyMedium (text splitLine)
    slider @"Split between" {} # suffixed ( Pipeline.do
      bodyMedium (text tipAmountLine)
      bodyMedium (text totalLine)
      bodyMedium (text perPersonLine) )
  # looped
    @( "Bill amount" :: String
     , "Tip percentage" :: { current :: Number, min :: Number, max :: Number, step :: [ discrete :: Number, continuous :: {} ] }
     , "Split between" :: { current :: Number, min :: Number, max :: Number, step :: [ discrete :: Number, continuous :: {} ] }
     ) # with dinnerBill
