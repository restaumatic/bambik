module PasswordGeneratorMDC3 (passwordGeneratorMDC3) where

import Prelude (identity, (#), ($), (>>>), Unit)

import Data.Variant (match)
import Effect (Effect)
import PasswordGeneratorLogic (rememberPassword, samplePassword, strengthText, strongMixRecipe)
import PUI (action, asCase, asField, completed, forField, mvu, atCase, projected, tapped, updated)
import PUI.Web.HTML (attr, body, div, staticText, text)
import PUI.Web.MDC3 (bodyMedium, button, card, elevation5, indeterminateLinearProgress, slider, toggleSwitch)
import Data.Profunctor.Row.RecordToRecord as RecordToRecord
import QualifiedDo.Semigroupoid as Semigroupoid

passwordGeneratorMDC3 :: Effect Unit
passwordGeneratorMDC3 =
  body $
    elevation5 $
      card { caption: "Password Generator" } $ ( Semigroupoid.do
          ( RecordToRecord.do
              slider { label: "Length" } # asField @"value" @"length"
              toggleSwitch { label: "Uppercase letters" } # asField @"value" @"uppercase"
              toggleSwitch { label: "Lowercase letters" } # asField @"value" @"lowercase"
              toggleSwitch { label: "Digits" } # asField @"value" @"digits"
              toggleSwitch { label: "Symbols" } # asField @"value" @"symbols") # completed
          bodyMedium ( RecordToRecord.do
              staticText "Strength: "
              text # projected @"value" strengthText ) # tapped
          div >>> attr "style" "font-family: monospace; word-break: break-all;" >>> attr "id" "password" $
            text # forField @"password" identity # tapped
          ( Semigroupoid.do
              button { label: "Generate" } # asCase @"clicked" @"generate"
              indeterminateLinearProgress # action samplePassword # atCase @"generate") # updated (match { generated: rememberPassword })
      ) # mvu strongMixRecipe
