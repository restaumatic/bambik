module Calculator (calculator) where

import Prelude ((#), ($), (&&), (<$>), (<<<), (<>), (==), (/=), (+), (-), (*), (/), (>>>), Unit, show)

import Data.Array (elem)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Number (fromString)
import Data.Profunctor (lcmap, rmap)
import Data.String (Pattern(..), contains, stripPrefix, stripSuffix)
import Data.Variant (match)
import Effect (Effect)
import PUI (constantly, displayed, foreach, forField, forValue, mvu, projection, toCase, updates)
import PUI.HTML (attrWith, body, clicked, div, provided, staticText, text, (:=))
import PUI.MDC (card, elevation20)
import QualifiedDo.Semigroupoid as Semigroupoid

calculator :: Effect Unit
calculator =
  body $
    elevation20 $
      card { caption: "Calculator" } $
        ( div >>> "style" := "display: inline-block;" $
            ( div >>> "style" := "width: 296px;" $ Semigroupoid.do
                div >>> "style"
                  := ( "height: 56px; display: flex; align-items: center; justify-content: flex-end; "
                        <> "padding: 0 16px; margin-bottom: 8px; border-radius: 4px; background: #263238; "
                        <> "color: #eceff1; font-size: 28px; font-family: Roboto Mono, monospace; overflow: hidden;" ) $ Semigroupoid.do
                    staticText "Error" # provided # lcmap faultyTally # displayed
                    text # forValue # forField @"entry" # provided # lcmap currentEntry
                div >>> "style" := "display: grid; grid-template-columns: repeat(4, 1fr); gap: 6px;" $
                  clicked ( div >>> attrWith "style" (keyStyle <<< _.key) $ text # projection _.key ) # foreach @"key" # constantly keyPad # rmap _.key) # toCase @"keyPressed"
        ) # updates (match { keyPressed: pressKey }) # mvu blankTally

keyStyle :: String -> String
keyStyle key =
  "height: 52px; display: flex; align-items: center; justify-content: center; "
    <> "font-size: 22px; font-family: Roboto, sans-serif; cursor: pointer; "
    <> "border-radius: 4px; user-select: none; "
    <> if key `elem` operatorKeys then "background: #ffab40; color: #263238;"
       else if key `elem` [ "C", "±" ] then "background: #b0bec5; color: #263238;"
       else "background: #eceff1; color: #263238;"

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

blankTally :: { total :: Number, operation :: Maybe String, entry :: String, entering :: Boolean, faulty :: Boolean }
blankTally = { total: 0.0, operation: Nothing, entry: "0", entering: false, faulty: false }

faultyTally :: { faulty :: Boolean } -> Maybe {}
faultyTally tally = if tally.faulty then Just {} else Nothing

currentEntry :: { faulty :: Boolean, entry :: String } -> Maybe { entry :: String }
currentEntry tally = if tally.faulty then Nothing else Just { entry: tally.entry }

pressKey
  :: String
  -> { total :: Number, operation :: Maybe String, entry :: String, entering :: Boolean, faulty :: Boolean }
  -> { total :: Number, operation :: Maybe String, entry :: String, entering :: Boolean, faulty :: Boolean }
pressKey key tally
  | tally.faulty && key /= "C" = pressKey key blankTally
  | key == "C" = blankTally
  | key == "±" = tally { entry = negated tally.entry }
  | key == "." && tally.entering =
      if contains (Pattern ".") tally.entry then tally else tally { entry = tally.entry <> "." }
  | key == "." = tally { entry = "0.", entering = true }
  | key `elem` operatorKeys = case settle { total: tally.total, operation: tally.operation, entry: tally.entry, entering: tally.entering } of
      Just total -> tally
        { total = total
        , operation = if key == "=" then Nothing else Just key
        , entry = format total
        , entering = false
        }
      Nothing -> blankTally { faulty = true }
  | tally.entering = tally { entry = if tally.entry == "0" then key else tally.entry <> key }
  | true = tally { entry = key, entering = true }

settle :: { total :: Number, operation :: Maybe String, entry :: String, entering :: Boolean } -> Maybe Number
settle tally = case tally.operation of
  Just operation | tally.entering -> compute operation tally.total (entryValue { entry: tally.entry })
  _ -> Just (entryValue { entry: tally.entry })

compute :: String -> Number -> Number -> Maybe Number
compute "+" a b = Just (a + b)
compute "−" a b = Just (a - b)
compute "×" a b = Just (a * b)
compute "÷" _ 0.0 = Nothing
compute "÷" a b = Just (a / b)
compute _ _ b = Just b

entryValue :: { entry :: String } -> Number
entryValue tally = fromMaybe 0.0 (fromString tally.entry)

negated :: String -> String
negated entry = case stripPrefix (Pattern "-") entry of
  Just positive -> positive
  Nothing -> "-" <> entry

format :: Number -> String
format n = fromMaybe shown (stripSuffix (Pattern ".0") shown)
  where
  shown = show n
