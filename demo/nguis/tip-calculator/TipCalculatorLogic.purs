module TipCalculatorLogic (dinnerBill, perPersonText, tipAmountText, totalText, whole) where

import Prelude ((*), (+), (/), (<$>))

import Data.Maybe (Maybe(..), maybe)
import Data.Number (fromString)
import Data.Number.Format (fixed, toStringWith)

dinnerBill :: { "Bill amount" :: String, "Tip percentage" :: { current :: Number, min :: Number, max :: Number, step :: Maybe Number }, "Split between" :: { current :: Number, min :: Number, max :: Number, step :: Maybe Number } }
dinnerBill = { "Bill amount": "", "Tip percentage": { current: 15.0, min: 0.0, max: 30.0, step: Just 1.0 }, "Split between": { current: 2.0, min: 1.0, max: 10.0, step: Just 1.0 } }

whole :: { current :: Number, min :: Number, max :: Number, step :: Maybe Number } -> String
whole { current } = toStringWith (fixed 0) current

tipAmountText :: { "Bill amount" :: String, "Tip percentage" :: { current :: Number, min :: Number, max :: Number, step :: Maybe Number } } -> String
tipAmountText { "Bill amount": amount, "Tip percentage": tipPercent } = money (tipAmount { "Bill amount": amount, "Tip percentage": tipPercent })

totalText :: { "Bill amount" :: String, "Tip percentage" :: { current :: Number, min :: Number, max :: Number, step :: Maybe Number } } -> String
totalText { "Bill amount": amount, "Tip percentage": tipPercent } = money (total { "Bill amount": amount, "Tip percentage": tipPercent })

perPersonText :: { "Bill amount" :: String, "Tip percentage" :: { current :: Number, min :: Number, max :: Number, step :: Maybe Number }, "Split between" :: { current :: Number, min :: Number, max :: Number, step :: Maybe Number } } -> String
perPersonText { "Bill amount": amount, "Tip percentage": tipPercent, "Split between": people } = money ((_ / people.current) <$> total { "Bill amount": amount, "Tip percentage": tipPercent })

tipAmount :: { "Bill amount" :: String, "Tip percentage" :: { current :: Number, min :: Number, max :: Number, step :: Maybe Number } } -> Maybe Number
tipAmount { "Bill amount": amount, "Tip percentage": tipPercent } = (\a -> a * tipPercent.current / 100.0) <$> fromString amount

total :: { "Bill amount" :: String, "Tip percentage" :: { current :: Number, min :: Number, max :: Number, step :: Maybe Number } } -> Maybe Number
total { "Bill amount": amount, "Tip percentage": tipPercent } = (\a -> a * (1.0 + tipPercent.current / 100.0)) <$> fromString amount

money :: Maybe Number -> String
money = maybe "—" (toStringWith (fixed 2))
