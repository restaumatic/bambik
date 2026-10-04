module PasswordGeneratorMDC3 (passwordGeneratorMDC3) where

import Prelude (Unit, (#), ($), (>>>))

import Effect (Effect)
import PasswordGeneratorViewModel (passwordText, samplePassword, strengthLine, strongMixRecipe)
import PUI (action, atCase, mvu)
import PUI.Web (attr, shown, text)
import PUI.Web.HTML (code)
import PUI.Web.MDC3 (body, bodyMedium, button, indeterminateLinearProgress, slider, toggleSwitch)
import QualifiedDo.Semigroupoid as Semigroupoid

passwordGeneratorMDC3 :: Effect Unit
passwordGeneratorMDC3 =
  body $
    ( Semigroupoid.do
      slider @"Length" {}
      toggleSwitch @"Uppercase letters" {}
      toggleSwitch @"Lowercase letters" {}
      toggleSwitch @"Digits" {}
      toggleSwitch @"Symbols" {}
      bodyMedium (text strengthLine) # shown
      code >>> attr "style" "word-break: break-all;" $ text passwordText # shown
      button @"Generate" {}
      indeterminateLinearProgress @"Generating password" # action @{ "Length" :: { current :: Number, min :: Number, max :: Number, step :: [ discrete :: Number, continuous :: {} ] } , "Uppercase letters" :: Boolean , "Lowercase letters" :: Boolean , "Digits" :: Boolean , "Symbols" :: Boolean , password :: String } samplePassword # atCase @"Generate"
    ) # mvu
      @( "Length" :: { current :: Number, min :: Number, max :: Number, step :: [ discrete :: Number, continuous :: {} ] }
       , "Uppercase letters" :: Boolean
       , "Lowercase letters" :: Boolean
       , "Digits" :: Boolean
       , "Symbols" :: Boolean
       , password :: String
       )
      strongMixRecipe
