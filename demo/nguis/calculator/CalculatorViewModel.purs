module CalculatorViewModel (blankTally, faultLine, functionKeys, keyEnteredLine, keyPad, operatorKeys, pressKey, readout) where

import Prelude ((&&), (<$>), (<>), (==), (/=), (+), (-), (*), (/), otherwise, show)

import Data.Array (elem)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Number (fromString)
import Data.String (Pattern(..), contains, stripPrefix, stripSuffix)
import Data.Variant (match)

blankTally :: { condition :: [ faulty :: {}, sound :: {} ], entry :: String, input :: [ entering :: {}, settled :: {} ], operation :: [ none :: {}, pending :: { key :: String } ], total :: Number }
blankTally = { total: 0.0, operation: .none {}, entry: "0", input: .settled {}, condition: .sound {} }

keyPad :: Array { key :: String }
keyPad = { key: _ } <$>
  [ "C", "±", "÷", "×"
  , "7", "8", "9", "−"
  , "4", "5", "6", "+"
  , "1", "2", "3", "="
  , "0", "."
  ]

operatorKeys :: Array String
operatorKeys = [ "÷", "×", "−", "+", "=" ]

functionKeys :: Array String
functionKeys = [ "C", "±" ]

readout :: { condition :: [ faulty :: {}, sound :: {} ], entry :: String, input :: [ entering :: {}, settled :: {} ], operation :: [ none :: {}, pending :: { key :: String } ], total :: Number } -> [ faulty :: {}, sound :: { entry :: String } ]
readout { condition, entry } = match { sound: \_ -> .sound { entry }, faulty: \_ -> .faulty {} } condition

pressKey :: { event :: String, model :: { condition :: [ faulty :: {}, sound :: {} ], entry :: String, input :: [ entering :: {}, settled :: {} ], operation :: [ none :: {}, pending :: { key :: String } ], total :: Number } } -> { condition :: [ faulty :: {}, sound :: {} ], entry :: String, input :: [ entering :: {}, settled :: {} ], operation :: [ none :: {}, pending :: { key :: String } ], total :: Number }
pressKey { event: key, model: tally@{ entry, operation, input } }
  | match { faulty: \_ -> key /= "C", sound: \_ -> false } tally.condition = pressKey { event: key, model: cleared tally }
  | key == "C" = cleared tally
  | key == "±" = tally { entry = negated entry }
  | key == "." && typing input =
    if contains (Pattern ".") entry then tally else tally { entry = entry <> "." }
  | key == "." = tally { entry = "0.", input = .entering {} }
  | key `elem` operatorKeys = case settle { total: tally.total, operation, entry, input } of
    Just total -> tally
      { total = total
      , operation = if key == "=" then .none {} else .pending { key }
      , entry = format total
      , input = .settled {}
      }
    Nothing -> (cleared tally) { condition = .faulty {} }
  | typing input = tally { entry = if entry == "0" then key else entry <> key }
  | otherwise = tally { entry = key, input = .entering {} }

cleared :: forall r. { total :: Number, operation :: [ pending :: { key :: String }, none :: {} ], entry :: String, input :: [ entering :: {}, settled :: {} ], condition :: [ sound :: {}, faulty :: {} ] | r } -> { total :: Number, operation :: [ pending :: { key :: String }, none :: {} ], entry :: String, input :: [ entering :: {}, settled :: {} ], condition :: [ sound :: {}, faulty :: {} ] | r }
cleared t = t { total = blankTally.total, operation = blankTally.operation, entry = blankTally.entry, input = blankTally.input, condition = blankTally.condition }

typing :: [ entering :: {}, settled :: {} ] -> Boolean
typing = match { entering: \_ -> true, settled: \_ -> false }

settle :: forall r1. { total :: Number, operation :: [ pending :: { key :: String }, none :: {} ], entry :: String, input :: [ entering :: {}, settled :: {} ] | r1 } -> Maybe Number
settle { operation, input, total, entry } = match
  { pending: \p -> if typing input then compute p.key total (entryValue { entry }) else Just (entryValue { entry })
  , none: \_ -> Just (entryValue { entry })
  } operation

compute :: String -> Number -> Number -> Maybe Number
compute "+" a b = Just (a + b)
compute "−" a b = Just (a - b)
compute "×" a b = Just (a * b)
compute "÷" _ 0.0 = Nothing
compute "÷" a b = Just (a / b)
compute _ _ b = Just b

entryValue :: forall r1. { entry :: String | r1 } -> Number
entryValue { entry } = fromMaybe 0.0 (fromString entry)

negated :: String -> String
negated entry = case stripPrefix (Pattern "-") entry of
  Just positive -> positive
  Nothing -> "-" <> entry

format :: Number -> String
format n = fromMaybe (show n) (stripSuffix (Pattern ".0") (show n))

faultLine :: {} -> String
faultLine _ = "Error"

keyEnteredLine :: { event :: String, model :: { condition :: [ faulty :: {}, sound :: {} ], entry :: String, input :: [ entering :: {}, settled :: {} ], operation :: [ none :: {}, pending :: { key :: String } ], total :: Number } } -> String
keyEnteredLine { event: key } = "Pressed " <> key
