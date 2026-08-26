module CalculatorMDC3 (calculatorMDC3) where

import Prelude (const, (#), ($), (<<<), (<>), (>>>), Unit)

import CalculatorLogic (blankTally, conditionOf, currentEntry, keyPad, operatorKeys, pressKey)
import Data.Array (elem)
import Data.Variant (match)
import Effect (Effect)
import PUI (foreach, mvu, forProperty, toCase, updated)
import PUI.Web.HTML (shownCase, attrWith, body, clicked, div, provided, staticText, text, (:=))
import PUI.Web.MDC3 (card, elevation5)
import QualifiedDo.Category as Category

calculatorMDC3 :: Effect Unit
calculatorMDC3 =
  body $
    elevation5 $
      card $
        ( ( div >>> "style" := "display: inline-block; width: 296px;" $ Category.do
                div >>> "style"
                  := ( "height: 56px; display: flex; align-items: center; justify-content: flex-end; "
                        <> "padding: 0 16px; margin-bottom: 8px; border-radius: 4px; background: #263238; "
                        <> "color: #eceff1; font-size: 28px; font-family: Roboto Mono, monospace; overflow: hidden;" ) $ Category.do
                    (staticText "Error") # shownCase @"faulty" conditionOf
                    text @"entry" # provided currentEntry
                div >>> "style" := "display: grid; grid-template-columns: repeat(4, 1fr); gap: 6px;" $
                  clicked ( div >>> attrWith "style" keyFace $ text @"key" # forProperty ) # foreach @"key" (const keyPad) ) # toCase @"keyPressed" _.key
        ) # updated (match { keyPressed: pressKey }) # mvu blankTally
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
