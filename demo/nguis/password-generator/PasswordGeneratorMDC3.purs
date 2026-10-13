module PasswordGeneratorMDC3 (passwordGeneratorMDC3) where

import Prelude (Unit, (#), ($), (>>>))

import Data.Profunctor.Row.RecordToRecord as RecordToRecord
import Effect (Effect)
import PasswordGeneratorViewModel (passwordText, samplePassword, strengthLine, strongMixRecipe)
import PUI (action, atCase, captured, looped, prefixed, unshown, with)
import PUI.Web (attr, text)
import PUI.Web.HTML (code)
import PUI.Web.MDC3 (body, bodyMedium, button, indeterminateLinearProgress, slider, toggleSwitch)
import QualifiedDo.Semigroupoid as Semigroupoid

passwordGeneratorMDC3 :: Effect Unit
passwordGeneratorMDC3 =
  body $ Semigroupoid.do
    slider @"Length"
    toggleSwitch @"Uppercase letters"
    toggleSwitch @"Lowercase letters"
    toggleSwitch @"Digits"
    toggleSwitch @"Symbols"
    prefixed ( RecordToRecord.do
      bodyMedium (text strengthLine)
      code >>> attr "style" "word-break: break-all;" $ text passwordText ) $ button @"Generate"
    indeterminateLinearProgress # action samplePassword # atCase @"Generate"
    unshown @"Password generated" # captured
  # looped
    @( "Length" :: { current :: Number, min :: Number, max :: Number, step :: [ discrete :: Number, continuous :: {} ] }
     , "Uppercase letters" :: Boolean
     , "Lowercase letters" :: Boolean
     , "Digits" :: Boolean
     , "Symbols" :: Boolean
     , password :: String
     ) # with strongMixRecipe
