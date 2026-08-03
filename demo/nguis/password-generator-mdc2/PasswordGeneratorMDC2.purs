module PasswordGeneratorMDC2 (passwordGeneratorMDC2) where

import Prelude (identity, (#), ($), (>>>), Unit)

import Data.Variant (match)
import Effect (Effect)
import PasswordGeneratorLogic (rememberPassword, samplePassword, strengthText, strongMixRecipe)
import PUI (action, asCase, asField, completed, forField, mvu, onCase, projected, tapped, updated)
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
              slider { label: "Length" } # asField @"length"
              toggleSwitch { label: "Uppercase letters" } # asField @"uppercase"
              toggleSwitch { label: "Lowercase letters" } # asField @"lowercase"
              toggleSwitch { label: "Digits" } # asField @"digits"
              toggleSwitch { label: "Symbols" } # asField @"symbols") # completed
          body2 ( RecordToRecord.do
              staticText "Strength: "
              text # projected strengthText ) # tapped
          div >>> attr "style" "font-family: monospace; word-break: break-all;" >>> attr "id" "password" $
            text # forField @"password" identity # tapped
          ( Semigroupoid.do
              button { label: "Generate" } # asCase @"generate"
              indeterminateLinearProgress # action samplePassword # onCase @"generate") # updated (match { generated: rememberPassword })
      ) # mvu strongMixRecipe
