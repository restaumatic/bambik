module TipCalculatorLogic (dinnerBill, perPersonText, tipAmountText, totalText, whole) where

import Prelude ((*), (+), (/), (<$>))

import Data.Maybe (Maybe(..), maybe)
import Data.Number (fromString)
import Data.Number.Format (fixed, toStringWith)

dinnerBill :: { amount :: String, tipPercent :: { current :: Number, min :: Number, max :: Number, step :: Maybe Number }, people :: { current :: Number, min :: Number, max :: Number, step :: Maybe Number } }
dinnerBill = { amount: "", tipPercent: { current: 15.0, min: 0.0, max: 30.0, step: Just 1.0 }, people: { current: 2.0, min: 1.0, max: 10.0, step: Just 1.0 } }

whole :: { current :: Number, min :: Number, max :: Number, step :: Maybe Number } -> String
whole { current } = toStringWith (fixed 0) current

tipAmountText :: { amount :: String, tipPercent :: { current :: Number, min :: Number, max :: Number, step :: Maybe Number } } -> String
tipAmountText { amount, tipPercent } = money (tipAmount { amount, tipPercent })

totalText :: { amount :: String, tipPercent :: { current :: Number, min :: Number, max :: Number, step :: Maybe Number } } -> String
totalText { amount, tipPercent } = money (total { amount, tipPercent })

perPersonText :: { amount :: String, tipPercent :: { current :: Number, min :: Number, max :: Number, step :: Maybe Number }, people :: { current :: Number, min :: Number, max :: Number, step :: Maybe Number } } -> String
perPersonText { amount, tipPercent, people } = money ((_ / people.current) <$> total { amount, tipPercent })

tipAmount :: { amount :: String, tipPercent :: { current :: Number, min :: Number, max :: Number, step :: Maybe Number } } -> Maybe Number
tipAmount { amount, tipPercent } = (\a -> a * tipPercent.current / 100.0) <$> fromString amount

total :: { amount :: String, tipPercent :: { current :: Number, min :: Number, max :: Number, step :: Maybe Number } } -> Maybe Number
total { amount, tipPercent } = (\a -> a * (1.0 + tipPercent.current / 100.0)) <$> fromString amount

money :: Maybe Number -> String
money = maybe "—" (toStringWith (fixed 2))
