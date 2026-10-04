module TipCalculatorViewModel (dinnerBill, perPersonLine, splitLine, tipAmountLine, tipLine, totalLine) where

import Prelude ((*), (+), (/), (<$>), (<>))

import Data.Maybe (Maybe, maybe)
import Data.Number (fromString)
import Data.Number.Format (fixed, toStringWith)

dinnerBill :: { "Bill amount" :: String, "Split between" :: { current :: Number, max :: Number, min :: Number, step :: [ continuous :: {}, discrete :: Number ] }, "Tip percentage" :: { current :: Number, max :: Number, min :: Number, step :: [ continuous :: {}, discrete :: Number ] } }
dinnerBill = { "Bill amount": "", "Tip percentage": { current: 15.0, min: 0.0, max: 30.0, step: .discrete 1.0 }, "Split between": { current: 2.0, min: 1.0, max: 10.0, step: .discrete 1.0 } }

tipLine :: { "Bill amount" :: String, "Split between" :: { current :: Number, max :: Number, min :: Number, step :: [ continuous :: {}, discrete :: Number ] }, "Tip percentage" :: { current :: Number, max :: Number, min :: Number, step :: [ continuous :: {}, discrete :: Number ] } } -> String
tipLine r = "Tip: " <> whole r."Tip percentage" <> "%"

splitLine :: { "Bill amount" :: String, "Split between" :: { current :: Number, max :: Number, min :: Number, step :: [ continuous :: {}, discrete :: Number ] }, "Tip percentage" :: { current :: Number, max :: Number, min :: Number, step :: [ continuous :: {}, discrete :: Number ] } } -> String
splitLine r = "Split between: " <> whole r."Split between" <> " people"

tipAmountLine :: { "Bill amount" :: String, "Split between" :: { current :: Number, max :: Number, min :: Number, step :: [ continuous :: {}, discrete :: Number ] }, "Tip percentage" :: { current :: Number, max :: Number, min :: Number, step :: [ continuous :: {}, discrete :: Number ] } } -> String
tipAmountLine r = "Tip amount: " <> money (tipAmount r)

totalLine :: { "Bill amount" :: String, "Split between" :: { current :: Number, max :: Number, min :: Number, step :: [ continuous :: {}, discrete :: Number ] }, "Tip percentage" :: { current :: Number, max :: Number, min :: Number, step :: [ continuous :: {}, discrete :: Number ] } } -> String
totalLine r = "Total: " <> money (total r)

perPersonLine :: { "Bill amount" :: String, "Split between" :: { current :: Number, max :: Number, min :: Number, step :: [ continuous :: {}, discrete :: Number ] }, "Tip percentage" :: { current :: Number, max :: Number, min :: Number, step :: [ continuous :: {}, discrete :: Number ] } } -> String
perPersonLine r = "Per person: " <> money ((_ / r."Split between".current) <$> total { "Bill amount": r."Bill amount", "Tip percentage": r."Tip percentage" })

whole :: forall r1. { current :: Number, min :: Number, max :: Number, step :: [ discrete :: Number, continuous :: {} ] | r1 } -> String
whole { current } = toStringWith (fixed 0) current

tipAmount :: forall r1. { "Bill amount" :: String, "Tip percentage" :: { current :: Number, min :: Number, max :: Number, step :: [ discrete :: Number, continuous :: {} ] } | r1 } -> Maybe Number
tipAmount r = (\a -> a * r."Tip percentage".current / 100.0) <$> fromString r."Bill amount"

total :: forall r1. { "Bill amount" :: String, "Tip percentage" :: { current :: Number, min :: Number, max :: Number, step :: [ discrete :: Number, continuous :: {} ] } | r1 } -> Maybe Number
total r = (\a -> a * (1.0 + r."Tip percentage".current / 100.0)) <$> fromString r."Bill amount"

money :: Maybe Number -> String
money = maybe "—" (toStringWith (fixed 2))
