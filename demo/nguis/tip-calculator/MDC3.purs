module TipCalculator.MDC3 (tipCalculator) where

import Prelude ((#), ($), Unit)

import Effect (Effect)
import PUI (looped, with)
import PUI.Web (text)
import PUI.Web.HTML (rangeInput)
import PUI.Web.MDC3 (body, bodyMedium, filledTextField, slider)
import QualifiedDo.Semigroupoid as Semigroupoid
import TipCalculator (dinnerBill, perPersonLine, splitLine, tipAmountLine, tipLine, totalLine)

tipCalculator :: Effect Unit
tipCalculator =
  body $ Semigroupoid.do
    filledTextField @"Bill amount"
    slider @"Tip percentage"
    rangeInput @"Tip percentage"
    bodyMedium $ text tipLine
    bodyMedium $ text splitLine
    slider @"Split between"
    bodyMedium $ text tipAmountLine
    bodyMedium $ text totalLine
    bodyMedium $ text perPersonLine
  # looped
    @( "Bill amount" :: String
     , "Tip percentage" :: { current :: Number, min :: Number, max :: Number, step :: [ discrete :: Number, continuous :: {} ] }
     , "Split between" :: { current :: Number, min :: Number, max :: Number, step :: [ discrete :: Number, continuous :: {} ] }
     ) # with dinnerBill
