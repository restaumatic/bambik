module TipCalculator (tipCalculator) where

import Prelude ((#), ($), (*), (+), (/), (<$>), Unit)

import Data.Maybe (Maybe, maybe)
import Data.Number (fromString)
import Data.Number.Format (fixed, toStringWith)
import Data.Profunctor.Row.RecordToRecord as RecordToRecord
import Effect (Effect)
import PUI (asField, completed, mvu, projection, tapped)
import PUI.HTML (body, staticText, text)
import PUI.MDC (body2, card, elevation20, filledTextField, slider)
import QualifiedDo.Semigroupoid as Semigroupoid

tipCalculator :: Effect Unit
tipCalculator =
  body $
    elevation20 $
      card { caption: "Tip Calculator" } $ ( Semigroupoid.do
          filledTextField { floatingLabel: "Bill amount" } # asField @"amount" # completed
          slider { label: "Tip percentage", min: minTipPercent, max: maxTipPercent, step: tipPercentStep } # asField @"tipPercent" # completed
          body2 ( RecordToRecord.do
              staticText "Tip: "
              text # projection tipPercentText
              staticText "%" ) # tapped
          body2 ( RecordToRecord.do
              staticText "Split between: "
              text # projection peopleText
              staticText " people" ) # tapped
          slider { label: "Split between", min: minPeople, max: maxPeople, step: peopleStep } # asField @"people" # completed
          body2 ( RecordToRecord.do
              staticText "Tip amount: "
              text # projection tipAmountText ) # tapped
          body2 ( RecordToRecord.do
              staticText "Total: "
              text # projection totalText ) # tapped
          body2 ( RecordToRecord.do
              staticText "Per person: "
              text # projection perPersonText ) # tapped
      ) # mvu dinnerBill

tipPercentText :: { tipPercent :: Number } -> String
tipPercentText bill = toStringWith (fixed 0) bill.tipPercent

peopleText :: { people :: Number } -> String
peopleText bill = toStringWith (fixed 0) bill.people

tipAmountText :: { amount :: String, tipPercent :: Number } -> String
tipAmountText bill = money (tipAmount bill)

totalText :: { amount :: String, tipPercent :: Number } -> String
totalText bill = money (total bill)

perPersonText :: { amount :: String, tipPercent :: Number, people :: Number } -> String
perPersonText bill = money ((_ / bill.people) <$> total { amount: bill.amount, tipPercent: bill.tipPercent })

tipAmount :: { amount :: String, tipPercent :: Number } -> Maybe Number
tipAmount bill = (\amount -> amount * bill.tipPercent / 100.0) <$> fromString bill.amount

total :: { amount :: String, tipPercent :: Number } -> Maybe Number
total bill = (\amount -> amount * (1.0 + bill.tipPercent / 100.0)) <$> fromString bill.amount

money :: Maybe Number -> String
money = maybe "—" (toStringWith (fixed 2))

dinnerBill :: { amount :: String, tipPercent :: Number, people :: Number }
dinnerBill = { amount: "", tipPercent: 15.0, people: 2.0 }

minTipPercent :: Number
minTipPercent = 0.0

maxTipPercent :: Number
maxTipPercent = 30.0

tipPercentStep :: Number
tipPercentStep = 1.0

minPeople :: Number
minPeople = 1.0

maxPeople :: Number
maxPeople = 10.0

peopleStep :: Number
peopleStep = 1.0
