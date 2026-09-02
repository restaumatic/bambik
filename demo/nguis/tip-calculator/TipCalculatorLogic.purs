module TipCalculatorLogic (dinnerBill, presentTips) where

import Prelude ((*), (+), (/), (<$>), (<>))

import Data.Maybe (Maybe, maybe)
import Data.Number (fromString)
import Data.Number.Format (fixed, toStringWith)

dinnerBill :: { "Bill amount" :: String, "Tip percentage" :: { current :: Number, min :: Number, max :: Number, step :: [ discrete :: Number, continuous :: {} ] }, "Split between" :: { current :: Number, min :: Number, max :: Number, step :: [ discrete :: Number, continuous :: {} ] }, tipLine :: String, splitLine :: String, tipAmountLine :: String, totalLine :: String, perPersonLine :: String }
dinnerBill = presentTips { "Bill amount": "", "Tip percentage": { current: 15.0, min: 0.0, max: 30.0, step: .discrete 1.0 }, "Split between": { current: 2.0, min: 1.0, max: 10.0, step: .discrete 1.0 }, tipLine: "", splitLine: "", tipAmountLine: "", totalLine: "", perPersonLine: "" }

presentTips :: { "Bill amount" :: String, "Tip percentage" :: { current :: Number, min :: Number, max :: Number, step :: [ discrete :: Number, continuous :: {} ] }, "Split between" :: { current :: Number, min :: Number, max :: Number, step :: [ discrete :: Number, continuous :: {} ] }, tipLine :: String, splitLine :: String, tipAmountLine :: String, totalLine :: String, perPersonLine :: String } -> { "Bill amount" :: String, "Tip percentage" :: { current :: Number, min :: Number, max :: Number, step :: [ discrete :: Number, continuous :: {} ] }, "Split between" :: { current :: Number, min :: Number, max :: Number, step :: [ discrete :: Number, continuous :: {} ] }, tipLine :: String, splitLine :: String, tipAmountLine :: String, totalLine :: String, perPersonLine :: String }
presentTips r = r
  { tipLine = "Tip: " <> whole r."Tip percentage" <> "%"
  , splitLine = "Split between: " <> whole r."Split between" <> " people"
  , tipAmountLine = "Tip amount: " <> money (tipAmount r."Bill amount" r."Tip percentage")
  , totalLine = "Total: " <> money (total r."Bill amount" r."Tip percentage")
  , perPersonLine = "Per person: " <> money ((_ / r."Split between".current) <$> total r."Bill amount" r."Tip percentage")
  }

whole :: { current :: Number, min :: Number, max :: Number, step :: [ discrete :: Number, continuous :: {} ] } -> String
whole { current } = toStringWith (fixed 0) current

tipAmount :: String -> { current :: Number, min :: Number, max :: Number, step :: [ discrete :: Number, continuous :: {} ] } -> Maybe Number
tipAmount amount tipPercent = (\a -> a * tipPercent.current / 100.0) <$> fromString amount

total :: String -> { current :: Number, min :: Number, max :: Number, step :: [ discrete :: Number, continuous :: {} ] } -> Maybe Number
total amount tipPercent = (\a -> a * (1.0 + tipPercent.current / 100.0)) <$> fromString amount

money :: Maybe Number -> String
money = maybe "—" (toStringWith (fixed 2))
