module TipCalculatorLogic (dinnerBill, perPersonText, tipAmountText, totalText, whole) where

import Prelude ((*), (+), (/), (<$>))

import Data.Maybe (Maybe(..), maybe)
import Data.Number (fromString)
import Data.Number.Format (fixed, toStringWith)

dinnerBill :: { amount :: String, "Tip percentage" :: { current :: Number, min :: Number, max :: Number, step :: Maybe Number }, "Split between" :: { current :: Number, min :: Number, max :: Number, step :: Maybe Number } }
dinnerBill = { amount: "", "Tip percentage": { current: 15.0, min: 0.0, max: 30.0, step: Just 1.0 }, "Split between": { current: 2.0, min: 1.0, max: 10.0, step: Just 1.0 } }

whole :: { current :: Number, min :: Number, max :: Number, step :: Maybe Number } -> String
whole { current } = toStringWith (fixed 0) current

tipAmountText :: { amount :: String, "Tip percentage" :: { current :: Number, min :: Number, max :: Number, step :: Maybe Number } } -> String
tipAmountText { amount, "Tip percentage": tipPercent } = money (tipAmount { amount, "Tip percentage": tipPercent })

totalText :: { amount :: String, "Tip percentage" :: { current :: Number, min :: Number, max :: Number, step :: Maybe Number } } -> String
totalText { amount, "Tip percentage": tipPercent } = money (total { amount, "Tip percentage": tipPercent })

perPersonText :: { amount :: String, "Tip percentage" :: { current :: Number, min :: Number, max :: Number, step :: Maybe Number }, "Split between" :: { current :: Number, min :: Number, max :: Number, step :: Maybe Number } } -> String
perPersonText { amount, "Tip percentage": tipPercent, "Split between": people } = money ((_ / people.current) <$> total { amount, "Tip percentage": tipPercent })

tipAmount :: { amount :: String, "Tip percentage" :: { current :: Number, min :: Number, max :: Number, step :: Maybe Number } } -> Maybe Number
tipAmount { amount, "Tip percentage": tipPercent } = (\a -> a * tipPercent.current / 100.0) <$> fromString amount

total :: { amount :: String, "Tip percentage" :: { current :: Number, min :: Number, max :: Number, step :: Maybe Number } } -> Maybe Number
total { amount, "Tip percentage": tipPercent } = (\a -> a * (1.0 + tipPercent.current / 100.0)) <$> fromString amount

money :: Maybe Number -> String
money = maybe "—" (toStringWith (fixed 2))
