module CalculatorMDC2 (calculatorMDC2) where

import Prelude (const, identity, (#), ($), (<<<), (<>), (>>>), Unit)

import CalculatorLogic (blankTally, conditionOf, currentEntry, keyPad, operatorKeys, pressKey)
import Data.Array (elem)
import Data.Variant (match)
import Effect (Effect)
import PUI (displayed, forField, foreach, mvu, forProperty, toCase, updated)
import PUI.Web.HTML (providedCase, attrWith, body, clicked, div, provided, staticText, text, (:=))
import PUI.Web.MDC2 (card, elevation20)
import QualifiedDo.Semigroupoid as Semigroupoid

calculatorMDC2 :: Effect Unit
calculatorMDC2 =
  body $
    elevation20 $
      card { caption: "Calculator" } $
        ( ( div >>> "style" := "display: inline-block; width: 296px;" $ Semigroupoid.do
                div >>> "style"
                  := ( "height: 56px; display: flex; align-items: center; justify-content: flex-end; "
                        <> "padding: 0 16px; margin-bottom: 8px; border-radius: 4px; background: #263238; "
                        <> "color: #eceff1; font-size: 28px; font-family: Roboto Mono, monospace; overflow: hidden;" ) $ Semigroupoid.do
                    staticText "Error" # providedCase @"faulty" conditionOf # displayed
                    text # forField @"entry" identity # provided currentEntry
                div >>> "style" := "display: grid; grid-template-columns: repeat(4, 1fr); gap: 6px;" $
                  clicked ( div >>> attrWith "style" keyFace $ text # forProperty @"value" @"key" identity ) # foreach @"key" (const keyPad) ) # toCase @"keyPressed" _.key
        ) # updated (match { keyPressed: pressKey }) # mvu blankTally

-- closed signature states the clicked content's row
keyFace :: { key :: String } -> String
keyFace = keyStyle <<< _.key

keyStyle :: String -> String
keyStyle key =
  "height: 52px; display: flex; align-items: center; justify-content: center; "
    <> "font-size: 22px; font-family: Roboto, sans-serif; cursor: pointer; "
    <> "border-radius: 4px; user-select: none; "
    <> if key `elem` operatorKeys then "background: #ffab40; color: #263238;"
       else if key `elem` [ "C", "±" ] then "background: #b0bec5; color: #263238;"
       else "background: #eceff1; color: #263238;"
