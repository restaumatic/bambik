module TipCalculatorMDC2 (tipCalculatorMDC2) where

import Prelude ((#), ($), Unit)

import Data.Profunctor.Row.RecordToRecord as RecordToRecord
import Effect (Effect)
import PUI (looped, prefixed, suffixed, with)
import PUI.Web (text)
import PUI.Web.HTML (rangeInput)
import PUI.Web.MDC2 (body, body2, filledTextField, slider)
import QualifiedDo.Semigroupoid as Semigroupoid
import TipCalculatorViewModel (dinnerBill, perPersonLine, splitLine, tipAmountLine, tipLine, totalLine)

tipCalculatorMDC2 :: Effect Unit
tipCalculatorMDC2 =
  body $ Semigroupoid.do
    filledTextField @"Bill amount"
    slider @"Tip percentage"
    rangeInput @"Tip percentage"
    prefixed ( RecordToRecord.do
      body2 (text tipLine)
      body2 (text splitLine) ) $ slider @"Split between" # suffixed ( RecordToRecord.do
      body2 (text tipAmountLine)
      body2 (text totalLine)
      body2 (text perPersonLine) )
  # looped
    @( "Bill amount" :: String
     , "Tip percentage" :: { current :: Number, min :: Number, max :: Number, step :: [ discrete :: Number, continuous :: {} ] }
     , "Split between" :: { current :: Number, min :: Number, max :: Number, step :: [ discrete :: Number, continuous :: {} ] }
     ) # with dinnerBill
