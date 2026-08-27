module CalculatorLogic (blankTally, keyPad, operatorKeys, pressKey, readout) where

import Prelude ((&&), (<$>), (<>), (==), (/=), (+), (-), (*), (/), show)

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

readout :: { condition :: [ sound :: {}, faulty :: {} ], entry :: String } -> [ sound :: { entry :: String }, faulty :: {} ]
readout { condition, entry } = match { sound: \_ -> .sound { entry }, faulty: \_ -> .faulty {} } condition

pressKey
  :: String
  -> { total :: Number, operation :: [ pending :: { key :: String }, none :: {} ], entry :: String, input :: [ entering :: {}, settled :: {} ], condition :: [ sound :: {}, faulty :: {} ] }
  -> { total :: Number, operation :: [ pending :: { key :: String }, none :: {} ], entry :: String, input :: [ entering :: {}, settled :: {} ], condition :: [ sound :: {}, faulty :: {} ] }
pressKey key tally@{ entry, operation, input }
  | match { faulty: \_ -> key /= "C", sound: \_ -> false } tally.condition = pressKey key blankTally
  | key == "C" = blankTally
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
      Nothing -> blankTally { condition = .faulty {} }
  | typing input = tally { entry = if entry == "0" then key else entry <> key }
  | true = tally { entry = key, input = .entering {} }

typing :: [ entering :: {}, settled :: {} ] -> Boolean
typing = match { entering: \_ -> true, settled: \_ -> false }

settle :: { total :: Number, operation :: [ pending :: { key :: String }, none :: {} ], entry :: String, input :: [ entering :: {}, settled :: {} ] } -> Maybe Number
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

entryValue :: { entry :: String } -> Number
entryValue { entry } = fromMaybe 0.0 (fromString entry)

negated :: String -> String
negated entry = case stripPrefix (Pattern "-") entry of
  Just positive -> positive
  Nothing -> "-" <> entry

format :: Number -> String
format n = fromMaybe (show n) (stripSuffix (Pattern ".0") (show n))
