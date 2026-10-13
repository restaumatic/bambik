module PasswordGeneratorMDC2 (passwordGeneratorMDC2) where

import Prelude (identity, Unit, (#), ($), (>>>))

import Data.Profunctor.Row.RecordToRecord as RecordToRecord
import Effect (Effect)
import PasswordGeneratorViewModel (passwordText, samplePassword, strengthLine, strongMixRecipe)
import PUI (action, atCase, fold, looped, prefixed, unshown, with)
import PUI.Web (attr, text)
import PUI.Web.HTML (code)
import PUI.Web.MDC2 (body, body2, button, indeterminateLinearProgress, slider, toggleSwitch)
import QualifiedDo.Semigroupoid as Semigroupoid

passwordGeneratorMDC2 :: Effect Unit
passwordGeneratorMDC2 =
  body $ Semigroupoid.do
    slider @"Length"
    toggleSwitch @"Uppercase letters"
    toggleSwitch @"Lowercase letters"
    toggleSwitch @"Digits"
    toggleSwitch @"Symbols"
    prefixed ( RecordToRecord.do
      body2 (text strengthLine)
      code >>> attr "style" "word-break: break-all;" $ text passwordText ) $ button @"Generate"
    indeterminateLinearProgress # action samplePassword # atCase @"Generate"
    unshown @"Password generated" # fold identity
  # looped
    @( "Length" :: { current :: Number, min :: Number, max :: Number, step :: [ discrete :: Number, continuous :: {} ] }
     , "Uppercase letters" :: Boolean
     , "Lowercase letters" :: Boolean
     , "Digits" :: Boolean
     , "Symbols" :: Boolean
     , password :: String
     ) # with strongMixRecipe
