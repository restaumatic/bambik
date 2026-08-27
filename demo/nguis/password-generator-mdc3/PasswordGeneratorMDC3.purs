module PasswordGeneratorMDC3 (passwordGeneratorMDC3) where

import Prelude (Unit, (#), ($), (>>>))

import Data.Variant (match)
import Effect (Effect)
import PasswordGeneratorLogic (rememberPassword, samplePassword, strengthText, strongMixRecipe)
import PUI (action, mvu, atCase, projected, updated)
import PUI.Web.HTML (shown, attr, body, div, staticText, text)
import PUI.Web.MDC3 (bodyMedium, button, card, elevation5, indeterminateLinearProgress, slider, toggleSwitch)
import Data.Profunctor.Row.RecordToRecord as RecordToRecord
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
          ( bodyMedium $ RecordToRecord.do
              staticText "Strength: "
              text @"strength" # projected strengthText ) # shown
          div >>> attr "style" "font-family: monospace; word-break: break-all;" >>> attr "id" "password" $
            (text @"password") # shown
          ( Category.do
              button @"Generate" {}
              indeterminateLinearProgress @"busy" # action samplePassword # atCase @"Generate" ) # updated (match { generated: rememberPassword })
      ) # mvu strongMixRecipe
