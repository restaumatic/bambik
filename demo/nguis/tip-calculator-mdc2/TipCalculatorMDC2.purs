module TipCalculatorMDC2 (tipCalculatorMDC2) where

import Prelude ((#), ($), (*), (+), (/), (<$>), Unit)

import Data.Maybe (Maybe(..), maybe)
import Data.Number (fromString)
import Data.Number.Format (fixed, toStringWith)
import Data.Profunctor.Row.RecordToRecord as RecordToRecord
import Effect (Effect)
import PUI (asField, completed, forField, mvu, projected, tapped)
import PUI.Web.HTML (body, staticText, text)
import PUI.Web.MDC2 (body2, card, elevation20, filledTextField, slider)
import QualifiedDo.Semigroupoid as Semigroupoid

tipCalculatorMDC2 :: Effect Unit
tipCalculatorMDC2 =
  body $
    elevation20 $
      card { caption: "Tip Calculator" } $ ( Semigroupoid.do
          filledTextField { floatingLabel: "Bill amount" } # asField @"amount" # completed
          slider { label: "Tip percentage" } # asField @"tipPercent" # completed
          body2 ( RecordToRecord.do
              staticText "Tip: "
              text # forField @"tipPercent" whole
              staticText "%" ) # tapped
          body2 ( RecordToRecord.do
              staticText "Split between: "
              text # forField @"people" whole
              staticText " people" ) # tapped
          slider { label: "Split between" } # asField @"people" # completed
          body2 ( RecordToRecord.do
              staticText "Tip amount: "
              text # projected tipAmountText ) # tapped
          body2 ( RecordToRecord.do
              staticText "Total: "
              text # projected totalText ) # tapped
          body2 ( RecordToRecord.do
              staticText "Per person: "
              text # projected perPersonText ) # tapped
      ) # mvu dinnerBill

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

dinnerBill :: { amount :: String, tipPercent :: { current :: Number, min :: Number, max :: Number, step :: Maybe Number }, people :: { current :: Number, min :: Number, max :: Number, step :: Maybe Number } }
dinnerBill = { amount: "", tipPercent: { current: 15.0, min: 0.0, max: 30.0, step: Just 1.0 }, people: { current: 2.0, min: 1.0, max: 10.0, step: Just 1.0 } }
