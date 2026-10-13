module CalculatorMDC2 (calculatorMDC2) where

import Prelude (const, (#), ($), (<>), (>>>), Unit)

import CalculatorViewModel (blankTally, functionKeys, keyPad, operatorKeys, pressKey)
import Data.Array (elem)
import Effect (Effect)
import PUI (anyOf, fold, joined, looped, unshown, with)
import PUI.Web (attrWith, clicked, inCaseAt, staticText, textAt, (:=))
import PUI.Web.HTML (div)
import PUI.Web.MDC2 (body)
import QualifiedDo.Semigroupoid as Semigroupoid

calculatorMDC2 :: Effect Unit
calculatorMDC2 =
  body $
    ( div >>> "style" := "display: inline-block; width: 296px;" $ Semigroupoid.do
      div >>> "style"
        := ( "height: 56px; display: flex; align-items: center; justify-content: flex-end; "
          <> "padding: 0 16px; margin-bottom: 8px; border-radius: 4px; background: #263238; "
          <> "color: #eceff1; font-size: 28px; font-family: Roboto Mono, monospace; overflow: hidden;" ) $ Semigroupoid.do
          staticText "Error" # inCaseAt @"faulty" @"condition"
          textAt @"entry" # inCaseAt @"sound" @"condition"
      ( div >>> "style" := "display: grid; grid-template-columns: repeat(4, 1fr); gap: 6px;" $
        clicked @"Key entered" @"key" ( div >>> attrWith "style" keyFace $ textAt @"key" ) # anyOf @"key" @( key :: String ) (const keyPad) ) # joined @"Key entered"
      unshown @"Key entered" # fold pressKey
    ) # looped
      @( total :: Number
       , operation :: [ pending :: { key :: String }, none :: {} ]
       , entry :: String
       , input :: [ entering :: {}, settled :: {} ]
       , condition :: [ sound :: {}, faulty :: {} ]
       ) # with blankTally

keyFace :: { key :: String } -> String
keyFace { key } =
  "height: 52px; display: flex; align-items: center; justify-content: center; "
    <> "font-size: 22px; font-family: Roboto, sans-serif; cursor: pointer; "
    <> "border-radius: 4px; user-select: none; "
    <> if key `elem` operatorKeys then "background: #ffab40; color: #263238;"
      else if key `elem` functionKeys then "background: #b0bec5; color: #263238;"
      else "background: #eceff1; color: #263238;"
