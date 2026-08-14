module PasswordGeneratorMDC2 (passwordGeneratorMDC2) where

import Prelude (identity, (#), ($), (>>>), Unit)

import Data.Variant (match)
import Effect (Effect)
import PasswordGeneratorLogic (rememberPassword, samplePassword, strengthText, strongMixRecipe)
import PUI (action, asCase, completed, mvu, atCase, projected, tapped, updated)
import PUI.Web.HTML (attr, body, div, staticText, text)
import PUI.Web.MDC2 (body2, button, card, elevation20, indeterminateLinearProgress, slider, toggleSwitch)
import Data.Profunctor.Row.RecordToRecord as RecordToRecord
import QualifiedDo.Semigroupoid as Semigroupoid

passwordGeneratorMDC2 :: Effect Unit
passwordGeneratorMDC2 =
  body $
    elevation20 $
      card { caption: "Password Generator" } $ ( Semigroupoid.do
          ( RecordToRecord.do
              slider @"length" { label: "Length" }
              toggleSwitch @"uppercase" { label: "Uppercase letters" }
              toggleSwitch @"lowercase" { label: "Lowercase letters" }
              toggleSwitch @"digits" { label: "Digits" }
              toggleSwitch @"symbols" { label: "Symbols" }) # completed
          body2 ( RecordToRecord.do
              staticText "Strength: "
              text @"value" # projected @"value" strengthText ) # tapped
          div >>> attr "style" "font-family: monospace; word-break: break-all;" >>> attr "id" "password" $
            text @"password" # tapped
          ( Semigroupoid.do
              button { label: "Generate" } # asCase @"clicked" @"generate"
              indeterminateLinearProgress @"busy" # action samplePassword # atCase @"generate") # updated (match { generated: rememberPassword })
      ) # mvu strongMixRecipe
