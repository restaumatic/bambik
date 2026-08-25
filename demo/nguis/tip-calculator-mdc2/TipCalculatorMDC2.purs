module TipCalculatorMDC2 (tipCalculatorMDC2) where

import Prelude ((<>), identity, (#), ($), Unit)

import Data.Profunctor.Row.RecordToRecord as RecordToRecord
import Effect (Effect)
import PUI (projection, mvu, projected)
import PUI.Web.HTML (rangeInput, shownAs, body, staticText, text)
import PUI.Web.MDC2 (body2, card, elevation20, filledTextField, slider)
import QualifiedDo.Semigroupoid as Semigroupoid
import TipCalculatorLogic (dinnerBill, perPersonText, tipAmountText, totalText, whole)

tipCalculatorMDC2 :: Effect Unit
tipCalculatorMDC2 =
  body $
    elevation20 $
      card $ ( Semigroupoid.do
          ( filledTextField @"Bill amount" {}
              <> slider @"Tip percentage" {}
              <> rangeInput @"Tip percentage" )
          shownAs identity ( body2 $ RecordToRecord.do
              staticText "Tip: "
              text @"Tip percentage" # projection whole
              staticText "%" )
          shownAs identity ( body2 $ RecordToRecord.do
              staticText "Split between: "
              text @"Split between" # projection whole
              staticText " people" )
          slider @"Split between" {}
          shownAs identity ( body2 $ RecordToRecord.do
              staticText "Tip amount: "
              text @"tipAmount" # projected tipAmountText )
          shownAs identity ( body2 $ RecordToRecord.do
              staticText "Total: "
              text @"total" # projected totalText )
          shownAs identity ( body2 $ RecordToRecord.do
              staticText "Per person: "
              text @"perPerson" # projected perPersonText )
      ) # mvu dinnerBill
