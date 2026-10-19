module Calculator (Condition, Input, Key, Operation, Tally, blankTally, isFunctionKey, isOperatorKey, keyPad, pressKey) where

import Prelude ((&&), (<$>), (<>), (==), (+), (-), (*), (/), not, otherwise, (||))

import Data.Array (elem)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Number (fromString)
import Data.Number.Format (precision, toStringWith)
import Data.String (Pattern(..), contains, stripPrefix, stripSuffix)
import Data.Variant (match)

type Tally =
  { condition :: Condition
  , entry :: String
  , input :: Input
  , operation :: Operation
  , total :: Number
  }

type Condition = [ faulty :: {}, sound :: {} ]

type Input = [ entering :: {}, settled :: {} ]

type Operation = [ none :: {}, pending :: Key ]

type Key = { key :: String }

blankTally :: Tally
blankTally = { total: 0.0, operation: .none {}, entry: "0", input: .settled {}, condition: .sound {} }

keyPad :: Array Key
keyPad = { key: _ } <$>
  [ "C", "±", "÷", "×"
  , "7", "8", "9", "−"
  , "4", "5", "6", "+"
  , "1", "2", "3", "="
  , "0", "."
  ]

isOperatorKey :: Key -> Boolean
isOperatorKey { key } = key `elem` operatorKeys

isFunctionKey :: Key -> Boolean
isFunctionKey { key } = key `elem` functionKeys

operatorKeys :: Array String
operatorKeys = [ "÷", "×", "−", "+", "=" ]

functionKeys :: Array String
functionKeys = [ "C", "±" ]

pressKey :: { event :: String, model :: Tally } -> Tally
pressKey { event: key, model: tally@{ entry, operation, input } }
  | match { faulty: \_ -> true, sound: \_ -> false } tally.condition = pressKey { event: key, model: blankTally }
  | key == "C" = blankTally
  | key == "±" = tally { entry = negated entry, input = match { pending: \_ -> .entering {}, none: \_ -> input } operation }
  | key == "." && typing input =
    if contains (Pattern ".") entry then tally else tally { entry = entry <> "." }
  | key == "." = tally { entry = "0.", input = .entering {} }
  | key `elem` operatorKeys = case settle tally of
    Just total -> tally
      { total = total
      , operation = if key == "=" then .none {} else .pending { key }
      , entry = format total
      , input = .settled {}
      }
    Nothing -> blankTally { condition = .faulty {} }
  | typing input = tally { entry = if entry == "0" then key else entry <> key }
  | otherwise = tally { entry = key, input = .entering {} }

typing :: Input -> Boolean
typing = match { entering: \_ -> true, settled: \_ -> false }

settle :: Tally -> Maybe Number
settle { operation, input, total, entry } = match
  { pending: \p -> if typing input then compute p.key total (entryValue entry) else Just (entryValue entry)
  , none: \_ -> Just (entryValue entry)
  } operation

compute :: String -> Number -> Number -> Maybe Number
compute "+" a b = Just (a + b)
compute "−" a b = Just (a - b)
compute "×" a b = Just (a * b)
compute "÷" _ 0.0 = Nothing
compute "÷" a b = Just (a / b)
compute _ _ b = Just b

entryValue :: String -> Number
entryValue entry = fromMaybe 0.0 (fromString entry)

negated :: String -> String
negated entry = case stripPrefix (Pattern "-") entry of
  Just positive -> positive
  Nothing -> "-" <> entry

format :: Number -> String
format n = trimmed (toStringWith (precision 10) n)
  where
  trimmed digits
    | contains (Pattern "e") digits || not (contains (Pattern ".") digits) = digits
    | otherwise = withoutZeros digits
  withoutZeros digits = case stripSuffix (Pattern "0") digits of
    Just shorter -> withoutZeros shorter
    Nothing -> fromMaybe digits (stripSuffix (Pattern ".") digits)

