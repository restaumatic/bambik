module PasswordGeneratorMDC3 (passwordGeneratorMDC3) where

import Prelude (Unit, (#), ($), (>>>))

import Data.Variant (match)
import Effect (Effect)
import PasswordGeneratorLogic (passwordText, rememberPassword, samplePassword, strengthLine, strongMixRecipe)
import PUI (action, mvu, atCase, updated)
import PUI.Web.HTML (shown, attr, body, code, text)
import PUI.Web.MDC3 (bodyMedium, button, card, elevation5, indeterminateLinearProgress, slider, toggleSwitch)
import QualifiedDo.Category as Category

passwordGeneratorMDC3 :: Effect Unit
passwordGeneratorMDC3 =
  body $
    elevation5 $
      card $ ( Category.do
          slider @"Length" {}
          toggleSwitch @"Uppercase letters" {}
          toggleSwitch @"Lowercase letters" {}
          toggleSwitch @"Digits" {}
          toggleSwitch @"Symbols" {}
          bodyMedium (text strengthLine) # shown
          code >>> attr "style" "word-break: break-all;" $ text passwordText # shown
          ( Category.do
              button @"Generate" {}
              indeterminateLinearProgress @"busy" # action samplePassword # atCase @"Generate" ) # updated (match { generated: rememberPassword })
      ) # mvu strongMixRecipe
