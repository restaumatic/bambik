module PasswordGenerator.MDC3 (passwordGenerator) where

import Prelude (Unit, (#), ($), (>>>))

import Effect (Effect)
import PasswordGenerator (samplePassword, strengthLine, strongMixRecipe)
import PUI (action, capture, looped, with)
import PUI.Web (attr, text, textAt)
import PUI.Web.HTML (code)
import PUI.Web.MDC3 (body, bodyMedium, button, indeterminateLinearProgress, slider, toggleSwitch)
import QualifiedDo.Semigroupoid as Semigroupoid

passwordGenerator :: Effect Unit
passwordGenerator =
  body $ Semigroupoid.do
    ( Semigroupoid.do
      slider @"Length"
      toggleSwitch @"Uppercase letters"
      toggleSwitch @"Lowercase letters"
      toggleSwitch @"Digits"
      toggleSwitch @"Symbols"
    ) # looped
    bodyMedium $ text strengthLine
    code >>> attr "style" "word-break: break-all;" $ textAt @"password"
    button @"Generate"
    indeterminateLinearProgress # action @"Generate" samplePassword
    capture @"Password generated"
  # looped # with strongMixRecipe
