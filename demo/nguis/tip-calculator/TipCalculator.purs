module TipCalculator (tipCalculator) where

import Prelude ((#), ($), (*), (+), (/), (<>), (<$>), Unit)

import Data.Maybe (Maybe, maybe)
import Data.Number (fromString)
import Data.Number.Format (fixed, toStringWith)
import Effect (Effect)
import PUI (asField, completed, mvu, projection, tapped)
import PUI.HTML (body, text)
import PUI.MDC (body2, card, elevation20, filledTextField, slider)
import QualifiedDo.Semigroupoid as Semigroupoid

type Bill = { amount :: String, tipPercent :: Number, people :: Number }

tipCalculator :: Effect Unit
tipCalculator =
  body $
    elevation20 $
      card { caption: "Tip Calculator" } $ ( Semigroupoid.do
          filledTextField { floatingLabel: "Bill amount" } # asField @"amount" # completed
          slider { label: "Tip percentage", min: minTipPercent, max: maxTipPercent, step: tipPercentStep } # asField @"tipPercent" # completed
          body2 (text # projection tipPercentLine) # tapped
          body2 (text # projection peopleLine) # tapped
          slider { label: "Split between", min: minPeople, max: maxPeople, step: peopleStep } # asField @"people" # completed
          body2 (text # projection tipAmountLine) # tapped
          body2 (text # projection totalLine) # tapped
          body2 (text # projection perPersonLine) # tapped
      ) # mvu dinnerBill

tipPercentLine :: Bill -> String
tipPercentLine bill = "Tip: " <> toStringWith (fixed 0) bill.tipPercent <> "%"

peopleLine :: Bill -> String
peopleLine bill = "Split between: " <> toStringWith (fixed 0) bill.people <> " people"

tipAmountLine :: Bill -> String
tipAmountLine bill = "Tip amount: " <> money (tipAmount bill)

totalLine :: Bill -> String
totalLine bill = "Total: " <> money (total bill)

perPersonLine :: Bill -> String
perPersonLine bill = "Per person: " <> money ((_ / bill.people) <$> total bill)

tipAmount :: Bill -> Maybe Number
tipAmount bill = (\amount -> amount * bill.tipPercent / 100.0) <$> fromString bill.amount

total :: Bill -> Maybe Number
total bill = (\amount -> amount * (1.0 + bill.tipPercent / 100.0)) <$> fromString bill.amount

money :: Maybe Number -> String
money = maybe "—" (toStringWith (fixed 2))

dinnerBill :: Bill
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
