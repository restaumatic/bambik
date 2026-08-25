module PasswordGeneratorMDC3 (passwordGeneratorMDC3) where

import Prelude (identity, Unit, (#), ($), (>>>))

import Data.Variant (match)
import Effect (Effect)
import PasswordGeneratorLogic (rememberPassword, samplePassword, strengthText, strongMixRecipe)
import PUI (action, mvu, atCase, projected, updated)
import PUI.Web.HTML (shownAs, attr, body, div, staticText, text)
import PUI.Web.MDC3 (bodyMedium, button, card, elevation5, indeterminateLinearProgress, slider, toggleSwitch)
import Data.Profunctor.Row.RecordToRecord as RecordToRecord
import QualifiedDo.Semigroupoid as Semigroupoid

passwordGeneratorMDC3 :: Effect Unit
passwordGeneratorMDC3 =
  body $
    elevation5 $
      card $ ( Semigroupoid.do
          slider @"Length" {}
          toggleSwitch @"Uppercase letters" {}
          toggleSwitch @"Lowercase letters" {}
          toggleSwitch @"Digits" {}
          toggleSwitch @"Symbols" {}
          shownAs identity ( bodyMedium $ RecordToRecord.do
              staticText "Strength: "
              text @"strength" # projected strengthText )
          div >>> attr "style" "font-family: monospace; word-break: break-all;" >>> attr "id" "password" $
            shownAs identity (text @"password")
          ( Semigroupoid.do
              button @"Generate" {}
              indeterminateLinearProgress @"busy" # action samplePassword # atCase @"Generate") # updated (match { generated: rememberPassword })
      ) # mvu strongMixRecipe
