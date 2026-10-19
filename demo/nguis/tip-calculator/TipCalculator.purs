module TipCalculator (Bill, Quantity, dinnerBill, perPersonLine, splitLine, tipAmountLine, tipLine, totalLine) where

import Prelude ((*), (+), (/), (<$>), (<>), (==))

import Data.Maybe (Maybe, maybe)
import Data.Number (fromString)
import Data.Number.Format (fixed, toStringWith)

type Bill =
  { "Bill amount" :: String
  , "Tip percentage" :: Quantity
  , "Split between" :: Quantity
  }

type Quantity =
  { current :: Number
  , min :: Number
  , max :: Number
  , step :: [ discrete :: Number, continuous :: {} ]
  }

dinnerBill :: Bill
dinnerBill =
  { "Bill amount": ""
  , "Tip percentage": { current: 15.0, min: 0.0, max: 30.0, step: .discrete 1.0 }
  , "Split between": { current: 2.0, min: 1.0, max: 10.0, step: .discrete 1.0 }
  }

tipLine :: Bill -> String
tipLine r = "Tip: " <> whole r."Tip percentage" <> "%"

splitLine :: Bill -> String
splitLine r = "Split between: " <> whole r."Split between" <> if r."Split between".current == 1.0 then " person" else " people"

tipAmountLine :: Bill -> String
tipAmountLine r = "Tip amount: " <> money (tipAmount r)

totalLine :: Bill -> String
totalLine r = "Total: " <> money (total r)

perPersonLine :: Bill -> String
perPersonLine r = "Per person: " <> money ((_ / r."Split between".current) <$> total r)

whole :: Quantity -> String
whole { current } = toStringWith (fixed 0) current

tipAmount :: Bill -> Maybe Number
tipAmount r = (\a -> a * r."Tip percentage".current / 100.0) <$> fromString r."Bill amount"

total :: Bill -> Maybe Number
total r = (\a -> a * (1.0 + r."Tip percentage".current / 100.0)) <$> fromString r."Bill amount"

money :: Maybe Number -> String
money = maybe "—" (toStringWith (fixed 2))
