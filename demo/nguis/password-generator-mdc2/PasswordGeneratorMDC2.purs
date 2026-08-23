module PasswordGeneratorMDC2 (passwordGeneratorMDC2) where

import Prelude (identity, Unit, (#), ($), (>>>))

import Data.Variant (match)
import Effect (Effect)
import PasswordGeneratorLogic (rememberPassword, samplePassword, strengthText, strongMixRecipe)
import PUI (action, completed, mvu, atCase, projected, updated)
import PUI.Web.HTML (shownAs, attr, body, div, staticText, text)
import PUI.Web.MDC2 (body2, button, card, elevation20, indeterminateLinearProgress, slider, toggleSwitch)
import Data.Profunctor.Row.RecordToRecord as RecordToRecord
import QualifiedDo.Semigroupoid as Semigroupoid

passwordGeneratorMDC2 :: Effect Unit
passwordGeneratorMDC2 =
  body $
    elevation20 $
      card $ ( Semigroupoid.do
          ( RecordToRecord.do
              slider @"Length" {}
              toggleSwitch @"Uppercase letters" {}
              toggleSwitch @"Lowercase letters" {}
              toggleSwitch @"Digits" {}
              toggleSwitch @"Symbols" {}) # completed
          shownAs identity ( body2 $ RecordToRecord.do
              staticText "Strength: "
              text @"strength" # projected strengthText )
          div >>> attr "style" "font-family: monospace; word-break: break-all;" >>> attr "id" "password" $
            shownAs identity (text @"password")
          ( Semigroupoid.do
              button @"Generate" {}
              indeterminateLinearProgress @"busy" # action samplePassword # atCase @"Generate") # updated (match { generated: rememberPassword })
      ) # mvu strongMixRecipe
