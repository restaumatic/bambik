module PasswordGeneratorMDC2 (passwordGeneratorMDC2) where

import Prelude (Unit, (#), ($), (>>>))

import Data.Variant (match)
import Effect (Effect)
import PasswordGeneratorLogic (passwordText, rememberPassword, samplePassword, strengthLine, strongMixRecipe)
import PUI (action, mvu, atCase, updated)
import PUI.Web.HTML (shown, attr, body, div, text)
import PUI.Web.MDC2 (body2, button, card, elevation20, indeterminateLinearProgress, slider, toggleSwitch)
import QualifiedDo.Category as Category

passwordGeneratorMDC2 :: Effect Unit
passwordGeneratorMDC2 =
  body $
    elevation20 $
      card $ ( Category.do
          slider @"Length" {}
          toggleSwitch @"Uppercase letters" {}
          toggleSwitch @"Lowercase letters" {}
          toggleSwitch @"Digits" {}
          toggleSwitch @"Symbols" {}
          body2 (text strengthLine) # shown
          div >>> attr "style" "font-family: monospace; word-break: break-all;" >>> attr "id" "password" $
            (text passwordText) # shown
          ( Category.do
              button @"Generate" {}
              indeterminateLinearProgress @"busy" # action samplePassword # atCase @"Generate" ) # updated (match { generated: rememberPassword })
      ) # mvu strongMixRecipe
