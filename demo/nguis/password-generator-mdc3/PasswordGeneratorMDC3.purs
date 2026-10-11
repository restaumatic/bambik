module PasswordGeneratorMDC3 (passwordGeneratorMDC3) where

import Prelude (identity, Unit, (#), ($), (>>>))

import Effect (Effect)
import PUI.Pipeline as Pipeline
import PasswordGeneratorViewModel (passwordText, samplePassword, strengthLine, strongMixRecipe)
import PUI (action, atCase, blankStatus, fold, looped, with)
import PUI.Web (attr, text)
import PUI.Web.HTML (code)
import PUI.Web.MDC3 (body, bodyMedium, button, indeterminateLinearProgress, slider, toggleSwitch)

passwordGeneratorMDC3 :: Effect Unit
passwordGeneratorMDC3 =
  body $ Pipeline.do
    slider @"Length" {}
    toggleSwitch @"Uppercase letters" {}
    toggleSwitch @"Lowercase letters" {}
    toggleSwitch @"Digits" {}
    toggleSwitch @"Symbols" {}
    bodyMedium (text strengthLine)
    code >>> attr "style" "word-break: break-all;" $ text passwordText
    button @"Generate" {}
    indeterminateLinearProgress # action samplePassword # atCase @"Generate"
    blankStatus @"Password generated" # fold identity
  # looped
    @( "Length" :: { current :: Number, min :: Number, max :: Number, step :: [ discrete :: Number, continuous :: {} ] }
     , "Uppercase letters" :: Boolean
     , "Lowercase letters" :: Boolean
     , "Digits" :: Boolean
     , "Symbols" :: Boolean
     , password :: String
     ) # with strongMixRecipe
