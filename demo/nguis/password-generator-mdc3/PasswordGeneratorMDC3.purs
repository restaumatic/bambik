module PasswordGeneratorMDC3 (passwordGeneratorMDC3) where

import Prelude (Unit, (#), ($), (>>>))

import Data.Variant (match)
import Effect (Effect)
import PasswordGeneratorLogic (presentPassword, rememberPassword, samplePassword, strongMixRecipe)
import PUI (action, mvu, atCase, settled, updated)
import PUI.Web.HTML (shown, attr, body, div, text)
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
          bodyMedium (text @"strengthLine") # shown
          div >>> attr "style" "font-family: monospace; word-break: break-all;" >>> attr "id" "password" $
            (text @"password") # shown
          ( Category.do
              button @"Generate" {}
              indeterminateLinearProgress @"busy" # action samplePassword # atCase @"Generate" ) # updated (match { generated: rememberPassword })
      ) # settled presentPassword # mvu strongMixRecipe
