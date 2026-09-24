module PasswordGeneratorMDC2 (passwordGeneratorMDC2) where

import Prelude (Unit, (#), ($), (>>>))

import Data.Variant (match)
import Effect (Effect)
import PasswordGeneratorLogic (passwordText, rememberPassword, samplePassword, strengthLine, strongMixRecipe)
import PUI (action, mvu, atCase, updated)
import PUI.Web (attr, shown, text)
import PUI.Web.HTML (code)
import PUI.Web.MDC2 (body, body2, button, card, indeterminateLinearProgress, slider, toggleSwitch)
import QualifiedDo.Semigroupoid as Semigroupoid

passwordGeneratorMDC2 :: Effect Unit
passwordGeneratorMDC2 =
  body $
    card $ ( Semigroupoid.do
      slider @"Length" {}
      toggleSwitch @"Uppercase letters" {}
      toggleSwitch @"Lowercase letters" {}
      toggleSwitch @"Digits" {}
      toggleSwitch @"Symbols" {}
      body2 (text strengthLine) # shown
      code >>> attr "style" "word-break: break-all;" $ text passwordText # shown
      ( Semigroupoid.do
        button @"Generate" {}
        indeterminateLinearProgress @"busy" # action samplePassword # atCase @"Generate" ) # updated (match { generated: rememberPassword })
    ) # mvu strongMixRecipe
