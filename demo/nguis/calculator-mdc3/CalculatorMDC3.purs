module CalculatorMDC3 (calculatorMDC3) where

import Prelude (const, (#), ($), (<>), (>>>), Unit)

import CalculatorViewModel (blankTally, faultLine, functionKeys, keyPad, operatorKeys, pressKey, readout)
import Data.Array (elem)
import Data.Variant (match)
import Effect (Effect)
import PUI (foreach, mvu, updated, with)
import PUI.Web (attrWith, clicked, shownWhen, text, (:=))
import PUI.Web.HTML (div)
import PUI.Web.MDC3 (body)
import QualifiedDo.Semigroupoid as Semigroupoid

calculatorMDC3 :: Effect Unit
calculatorMDC3 =
  body $
    ( div >>> "style" := "display: inline-block; width: 296px;" $ Semigroupoid.do
      div >>> "style"
        := ( "height: 56px; display: flex; align-items: center; justify-content: flex-end; "
          <> "padding: 0 16px; margin-bottom: 8px; border-radius: 4px; background: #263238; "
          <> "color: #eceff1; font-size: 28px; font-family: Roboto Mono, monospace; overflow: hidden;" ) $ Semigroupoid.do
          text faultLine # shownWhen @"faulty" @( sound :: { entry :: String }, faulty :: {} ) readout
          text _.entry # shownWhen @"sound" readout
      ( div >>> "style" := "display: grid; grid-template-columns: repeat(4, 1fr); gap: 6px;" $
        clicked @"entered" _.key ( div >>> attrWith "style" keyFace $ text _.key ) # foreach @"key" @( key :: String ) (const keyPad) ) # with {} # updated (match { entered: pressKey })
    ) # mvu
      @( total :: Number
       , operation :: [ pending :: { key :: String }, none :: {} ]
       , entry :: String
       , input :: [ entering :: {}, settled :: {} ]
       , condition :: [ sound :: {}, faulty :: {} ]
       )
      blankTally

keyFace :: { key :: String } -> String
keyFace { key } =
  "height: 52px; display: flex; align-items: center; justify-content: center; "
    <> "font-size: 22px; font-family: Roboto, sans-serif; cursor: pointer; "
    <> "border-radius: 4px; user-select: none; "
    <> if key `elem` operatorKeys then "background: #ffab40; color: #263238;"
      else if key `elem` functionKeys then "background: #b0bec5; color: #263238;"
      else "background: #eceff1; color: #263238;"
