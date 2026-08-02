module CalculatorMDC2 (calculatorMDC2) where

import Prelude (identity, (#), ($), (&&), (<$>), (<<<), (<>), (==), (/=), (+), (-), (*), (/), (>>>), Unit, show)

import Data.Array (elem)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Number (fromString)
import Data.String (Pattern(..), contains, stripPrefix, stripSuffix)
import Data.Variant (match)
import Effect (Effect)
import PUI (constantly, displayed, forField, foreach, mvu, forProperty, toCase, updated)
import PUI.HTML (atCase, attrWith, body, clicked, div, provided, staticText, text, (:=))
import PUI.MDC2 (card, elevation20)
import QualifiedDo.Semigroupoid as Semigroupoid

calculatorMDC2 :: Effect Unit
calculatorMDC2 =
  body $
    elevation20 $
      card { caption: "Calculator" } $
        ( div >>> "style" := "display: inline-block;" $
            ( div >>> "style" := "width: 296px;" $ Semigroupoid.do
                div >>> "style"
                  := ( "height: 56px; display: flex; align-items: center; justify-content: flex-end; "
                        <> "padding: 0 16px; margin-bottom: 8px; border-radius: 4px; background: #263238; "
                        <> "color: #eceff1; font-size: 28px; font-family: Roboto Mono, monospace; overflow: hidden;" ) $ Semigroupoid.do
                    staticText "Error" # atCase @"faulty" conditionOf # displayed
                    text # forField @"entry" identity # provided currentEntry
                div >>> "style" := "display: grid; grid-template-columns: repeat(4, 1fr); gap: 6px;" $
                  clicked ( div >>> attrWith "style" (keyStyle <<< _.key) $ text # forProperty @"key" identity ) # foreach @"key" identity # constantly keyPad ) # toCase @"keyPressed" _.key
        ) # updated (match { keyPressed: pressKey }) # mvu blankTally

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

blankTally :: { total :: Number, operation :: Maybe String, entry :: String, entering :: Boolean, condition :: [ sound :: {}, faulty :: {} ] }
blankTally = { total: 0.0, operation: Nothing, entry: "0", entering: false, condition: .sound {} }

conditionOf :: { condition :: [ sound :: {}, faulty :: {} ] } -> [ sound :: {}, faulty :: {} ]
conditionOf { condition } = condition

currentEntry :: { condition :: [ sound :: {}, faulty :: {} ], entry :: String } -> Maybe { entry :: String }
currentEntry { condition, entry } = match { sound: \_ -> Just { entry }, faulty: \_ -> Nothing } condition

pressKey
  :: String
  -> { total :: Number, operation :: Maybe String, entry :: String, entering :: Boolean, condition :: [ sound :: {}, faulty :: {} ] }
  -> { total :: Number, operation :: Maybe String, entry :: String, entering :: Boolean, condition :: [ sound :: {}, faulty :: {} ] }
pressKey key tally@{ entry, entering, operation }
  | match { faulty: \_ -> key /= "C", sound: \_ -> false } tally.condition = pressKey key blankTally
  | key == "C" = blankTally
  | key == "±" = tally { entry = negated entry }
  | key == "." && entering =
      if contains (Pattern ".") entry then tally else tally { entry = entry <> "." }
  | key == "." = tally { entry = "0.", entering = true }
  | key `elem` operatorKeys = case settle { total: tally.total, operation, entry, entering } of
      Just total -> tally
        { total = total
        , operation = if key == "=" then Nothing else Just key
        , entry = format total
        , entering = false
        }
      Nothing -> blankTally { condition = .faulty {} }
  | entering = tally { entry = if entry == "0" then key else entry <> key }
  | true = tally { entry = key, entering = true }

settle :: { total :: Number, operation :: Maybe String, entry :: String, entering :: Boolean } -> Maybe Number
settle tally@{ entering, total, entry } = case tally.operation of
  Just operation | entering -> compute operation total (entryValue { entry })
  _ -> Just (entryValue { entry })

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
