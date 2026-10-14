module PasswordGenerator.MDC3 (passwordGenerator) where

import Prelude (Unit, (#), ($), (>>>))

import Effect (Effect)
import PasswordGenerator (passwordText, samplePassword, strengthLine, strongMixRecipe)
import PUI (action, atCase, capture, looped, with)
import PUI.Web (attr, text)
import PUI.Web.HTML (code)
import PUI.Web.MDC3 (body, bodyMedium, button, indeterminateLinearProgress, slider, toggleSwitch)
import QualifiedDo.Semigroupoid as Semigroupoid

passwordGenerator :: Effect Unit
passwordGenerator =
  body $ Semigroupoid.do
    slider @"Length"
    toggleSwitch @"Uppercase letters"
    toggleSwitch @"Lowercase letters"
    toggleSwitch @"Digits"
    toggleSwitch @"Symbols"
    bodyMedium $ text strengthLine
    code >>> attr "style" "word-break: break-all;" $ text passwordText
    button @"Generate"
    indeterminateLinearProgress # action samplePassword # atCase @"Generate"
    capture @"Password generated"
  # looped
    @( "Length" :: { current :: Number, min :: Number, max :: Number, step :: [ discrete :: Number, continuous :: {} ] }
     , "Uppercase letters" :: Boolean
     , "Lowercase letters" :: Boolean
     , "Digits" :: Boolean
     , "Symbols" :: Boolean
     , password :: String
     ) # with strongMixRecipe
