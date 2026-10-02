module PasswordGeneratorMDC2 (passwordGeneratorMDC2) where

import Prelude (Unit, (#), ($), (>>>))

import Data.Variant (match)
import Effect (Effect)
import PasswordGeneratorViewModel (passwordText, rememberPassword, samplePassword, strengthLine, strongMixRecipe)
import PUI (action, atCase, mvu, state, updated)
import PUI.Web (attr, shown, text)
import PUI.Web.HTML (code)
import PUI.Web.MDC2 (body, body2, button, indeterminateLinearProgress, slider, toggleSwitch)
import QualifiedDo.Semigroupoid as Semigroupoid

passwordGeneratorMDC2 :: Effect Unit
passwordGeneratorMDC2 =
  body $
    ( Semigroupoid.do
      state @"password" @String
      slider @"Length" {}
      toggleSwitch @"Uppercase letters" {}
      toggleSwitch @"Lowercase letters" {}
      toggleSwitch @"Digits" {}
      toggleSwitch @"Symbols" {}
      body2 (text strengthLine) # shown
      code >>> attr "style" "word-break: break-all;" $ text passwordText # shown
      ( Semigroupoid.do
        button @"Generate" {}
        indeterminateLinearProgress @"Generating password" # action @[ generated :: String ] samplePassword # atCase @"Generate" ) # updated (match { generated: rememberPassword })
    ) # mvu strongMixRecipe
