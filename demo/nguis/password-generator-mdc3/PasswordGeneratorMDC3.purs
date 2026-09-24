module PasswordGeneratorMDC3 (passwordGeneratorMDC3) where

import Prelude (Unit, (#), ($), (>>>))

import Data.Variant (match)
import Effect (Effect)
import PasswordGeneratorLogic (passwordText, rememberPassword, samplePassword, strengthLine, strongMixRecipe)
import PUI (action, mvu, atCase, updated)
import PUI.Web (attr, shown, text)
import PUI.Web.HTML (code)
import PUI.Web.MDC3 (body, bodyMedium, button, card, indeterminateLinearProgress, slider, toggleSwitch)
import QualifiedDo.Semigroupoid as Semigroupoid

passwordGeneratorMDC3 :: Effect Unit
passwordGeneratorMDC3 =
  body $
    card $ ( Semigroupoid.do
      slider @"Length" {}
      toggleSwitch @"Uppercase letters" {}
      toggleSwitch @"Lowercase letters" {}
      toggleSwitch @"Digits" {}
      toggleSwitch @"Symbols" {}
      bodyMedium (text strengthLine) # shown
      code >>> attr "style" "word-break: break-all;" $ text passwordText # shown
      ( Semigroupoid.do
        button @"Generate" {}
        indeterminateLinearProgress @"busy" # action samplePassword # atCase @"Generate" ) # updated (match { generated: rememberPassword })
    ) # mvu strongMixRecipe
