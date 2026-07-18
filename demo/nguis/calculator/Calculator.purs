module Calculator (calculator) where

import Prelude ((#), ($), (&&), (<>), (==), (/=), (+), (-), (*), (/), (>>>), Unit, show)

import Data.Array (elem)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Number (fromString)
import Data.Profunctor.Row.RecordToRecord as RecordToRecord
import Data.String (Pattern(..), contains, stripPrefix, stripSuffix)
import Data.Variant (match)
import Effect (Effect)
import PUI (PUI, mvu, toCase, updates)
import PUI.HTML (body, div, dynamic, each, onKeyClicked, staticText, (:=))
import PUI.Web (Web)
import PUI.MDC (card, elevation20)

calculator :: Effect Unit
calculator =
  body $
    elevation20 $
      card { caption: "Calculator" } $
        ( div >>> "style" := "display: inline-block;" $
            (onKeyClicked (dynamic renderCalculator) # toCase @"keyPressed")
        )
          # updates (match { keyPressed: pressKey })
          # mvu blankTally

renderCalculator :: Tally -> PUI Web {} {}
renderCalculator tally =
  div >>> "style" := "width: 296px;" $ RecordToRecord.do
    div >>> "style"
      := ( "height: 56px; display: flex; align-items: center; justify-content: flex-end; "
            <> "padding: 0 16px; margin-bottom: 8px; border-radius: 4px; background: #263238; "
            <> "color: #eceff1; font-size: 28px; font-family: Roboto Mono, monospace; overflow: hidden;"
        ) $ staticText (readout tally)
    div >>> "style" := "display: grid; grid-template-columns: repeat(4, 1fr); gap: 6px;" $
      each keyPad padButton
  where
  padButton key =
    div >>> "data-key" := key >>> "class" := "key"
      >>> "style"
        := ( "height: 52px; display: flex; align-items: center; justify-content: center; "
              <> "font-size: 22px; font-family: Roboto, sans-serif; cursor: pointer; "
              <> "border-radius: 4px; user-select: none; "
              <> if key `elem` operatorKeys then "background: #ffab40; color: #263238;"
                 else if key `elem` [ "C", "±" ] then "background: #b0bec5; color: #263238;"
                 else "background: #eceff1; color: #263238;"
          ) $ staticText key

keyPad :: Array String
keyPad =
  [ "C", "±", "÷", "×"
  , "7", "8", "9", "−"
  , "4", "5", "6", "+"
  , "1", "2", "3", "="
  , "0", "."
  ]

operatorKeys :: Array String
operatorKeys = [ "÷", "×", "−", "+", "=" ]

type Tally =
  { total :: Number
  , operation :: Maybe String
  , entry :: String
  , entering :: Boolean
  , faulty :: Boolean
  }

blankTally :: Tally
blankTally = { total: 0.0, operation: Nothing, entry: "0", entering: false, faulty: false }

readout :: Tally -> String
readout tally = if tally.faulty then "Error" else tally.entry

pressKey :: String -> Tally -> Tally
pressKey key tally
  | tally.faulty && key /= "C" = pressKey key blankTally
  | key == "C" = blankTally
  | key == "±" = tally { entry = negated tally.entry }
  | key == "." && tally.entering =
      if contains (Pattern ".") tally.entry then tally else tally { entry = tally.entry <> "." }
  | key == "." = tally { entry = "0.", entering = true }
  | key `elem` operatorKeys = case settle tally of
      Just total -> tally
        { total = total
        , operation = if key == "=" then Nothing else Just key
        , entry = format total
        , entering = false
        }
      Nothing -> blankTally { faulty = true }
  | tally.entering = tally { entry = if tally.entry == "0" then key else tally.entry <> key }
  | true = tally { entry = key, entering = true }

settle :: Tally -> Maybe Number
settle tally = case tally.operation of
  Just operation | tally.entering -> compute operation tally.total (entryValue tally)
  _ -> Just (entryValue tally)

compute :: String -> Number -> Number -> Maybe Number
compute "+" a b = Just (a + b)
compute "−" a b = Just (a - b)
compute "×" a b = Just (a * b)
compute "÷" _ 0.0 = Nothing
compute "÷" a b = Just (a / b)
compute _ _ b = Just b

entryValue :: Tally -> Number
entryValue tally = fromMaybe 0.0 (fromString tally.entry)

negated :: String -> String
negated entry = case stripPrefix (Pattern "-") entry of
  Just positive -> positive
  Nothing -> "-" <> entry

format :: Number -> String
format n = fromMaybe shown (stripSuffix (Pattern ".0") shown)
  where
  shown = show n
