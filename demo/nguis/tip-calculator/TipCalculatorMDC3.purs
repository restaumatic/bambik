module TipCalculatorMDC3 (tipCalculatorMDC3) where

import Prelude ((#), ($), Unit)

import Data.Profunctor.Row.RecordToRecord as RecordToRecord
import Effect (Effect)
import PUI (looped, prefixed, suffixed, with)
import PUI.Web (text)
import PUI.Web.HTML (rangeInput)
import PUI.Web.MDC3 (body, bodyMedium, filledTextField, slider)
import QualifiedDo.Semigroupoid as Semigroupoid
import TipCalculatorViewModel (dinnerBill, perPersonLine, splitLine, tipAmountLine, tipLine, totalLine)

tipCalculatorMDC3 :: Effect Unit
tipCalculatorMDC3 =
  body $ Semigroupoid.do
    filledTextField @"Bill amount"
    slider @"Tip percentage"
    rangeInput @"Tip percentage"
    prefixed ( RecordToRecord.do
      bodyMedium (text tipLine)
      bodyMedium (text splitLine) ) $ slider @"Split between" # suffixed ( RecordToRecord.do
      bodyMedium (text tipAmountLine)
      bodyMedium (text totalLine)
      bodyMedium (text perPersonLine) )
  # looped
    @( "Bill amount" :: String
     , "Tip percentage" :: { current :: Number, min :: Number, max :: Number, step :: [ discrete :: Number, continuous :: {} ] }
     , "Split between" :: { current :: Number, min :: Number, max :: Number, step :: [ discrete :: Number, continuous :: {} ] }
     ) # with dinnerBill
