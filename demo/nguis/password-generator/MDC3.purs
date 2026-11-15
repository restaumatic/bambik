module PasswordGenerator.MDC3 (passwordGenerator) where

import Prelude (Unit, (#), ($), (>>>))

import Effect (Effect)
import PasswordGenerator (characterKinds, samplePassword, strengthLine, strongMixRecipe)
import PUI (action, capture, looped, stepped, with)
import PUI.Web (attr, provided, text, textAt)
import PUI.Web.HTML (code)
import PUI.Web.MDC3 (body, bodyMedium, button, indeterminateLinearProgress, slider, toggleSwitch)
import QualifiedDo.Semigroupoid as Semigroupoid

passwordGenerator :: Effect Unit
passwordGenerator =
  body $ Semigroupoid.do
    slider @"Length"
    toggleSwitch @"Uppercase letters"
    toggleSwitch @"Lowercase letters"
    toggleSwitch @"Digits"
    toggleSwitch @"Symbols"
    bodyMedium $ text strengthLine
    code >>> attr "style" "word-break: break-all;" $ textAt @"password"
    ( Semigroupoid.do
      button @"Generate" # provided @"chosen" characterKinds
      indeterminateLinearProgress # action @"Generate" samplePassword
      capture @"Password generated" ) # stepped
  # looped # with strongMixRecipe
