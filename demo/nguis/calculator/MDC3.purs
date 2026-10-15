module Calculator.MDC3 (calculator) where

import Prelude (const, (#), ($), (<>), (>>>), Unit)

import Calculator (blankTally, isFunctionKey, isOperatorKey, keyPad, pressKey)
import Effect (Effect)
import PUI (anyOf, fold, joined, looped, with)
import PUI.Web (attrWith, inCaseAt, staticText, textAt, (:=))
import PUI.Web.HTML (div)
import PUI.Web.MDC3 (body, tile)
import QualifiedDo.Semigroupoid as Semigroupoid

calculator :: Effect Unit
calculator =
  body $ div >>> "style" := "display: inline-block; width: 296px;" $ Semigroupoid.do
    div >>> "style" := readoutFace $ Semigroupoid.do
      staticText "Error" # inCaseAt @"faulty" @"condition"
      textAt @"entry" # inCaseAt @"sound" @"condition"
    ( div >>> "style" := "display: grid; grid-template-columns: repeat(4, 1fr); gap: 6px;" $
      ( attrWith "style" keyFace $ tile @"Key entered" @"key" $ textAt @"key" ) # anyOf @"key" (const keyPad) ) # joined @"Key entered"
    fold @"Key entered" pressKey
  # looped # with blankTally

readoutFace :: String
readoutFace =
  "height: 56px; display: flex; align-items: center; justify-content: flex-end; "
    <> "padding: 0 16px; margin-bottom: 8px; border-radius: 4px; background: #263238; "
    <> "color: #eceff1; font-size: 28px; font-family: Roboto Mono, monospace; overflow: hidden;"

keyFace :: { key :: String } -> String
keyFace key =
  "height: 52px; display: flex; align-items: center; justify-content: center; "
    <> "font-size: 22px; font-family: Roboto, sans-serif; cursor: pointer; "
    <> "border-radius: 4px; user-select: none; "
    <> if isOperatorKey key then "background: #ffab40; color: #263238;"
      else if isFunctionKey key then "background: #b0bec5; color: #263238;"
      else "background: #eceff1; color: #263238;"
