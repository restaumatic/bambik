module CalculatorLogic (blankTally, faultLine, functionKeys, keyPad, operatorKeys, pressKey, readout) where

import Prelude ((&&), (<$>), (<>), (==), (/=), (+), (-), (*), (/), otherwise, show)

import Data.Array (elem)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Number (fromString)
import Data.String (Pattern(..), contains, stripPrefix, stripSuffix)
import Data.Variant (match)

blankTally :: { total :: Number, operation :: [ pending :: { key :: String }, none :: {} ], entry :: String, input :: [ entering :: {}, settled :: {} ], condition :: [ sound :: {}, faulty :: {} ] }
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

readout :: forall r1. { condition :: [ sound :: {}, faulty :: {} ], entry :: String | r1 } -> [ sound :: { entry :: String }, faulty :: {} ]
readout { condition, entry } = match { sound: \_ -> .sound { entry }, faulty: \_ -> .faulty {} } condition

pressKey :: forall r1. String -> { total :: Number, operation :: [ pending :: { key :: String }, none :: {} ], entry :: String, input :: [ entering :: {}, settled :: {} ], condition :: [ sound :: {}, faulty :: {} ] | r1 } -> { total :: Number, operation :: [ pending :: { key :: String }, none :: {} ], entry :: String, input :: [ entering :: {}, settled :: {} ], condition :: [ sound :: {}, faulty :: {} ] | r1 }
pressKey key tally@{ entry, operation, input }
  | match { faulty: \_ -> key /= "C", sound: \_ -> false } tally.condition = pressKey key (cleared tally)
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

-- the tally cleared, whatever else the row carries
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

faultLine :: forall r1. { | r1 } -> String
faultLine _ = "Error"
