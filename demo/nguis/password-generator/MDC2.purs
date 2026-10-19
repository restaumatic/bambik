module PasswordGenerator.MDC2 (passwordGenerator) where

import Prelude (Unit, (#), ($), (>>>))

import Effect (Effect)
import PasswordGenerator (characterKinds, samplePassword, strengthLine, strongMixRecipe)
import PUI (action, capture, looped, with)
import PUI.Web (attr, provided, text, textAt)
import PUI.Web.HTML (code)
import PUI.Web.MDC2 (body, body2, button, indeterminateLinearProgress, slider, toggleSwitch)
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
    body2 $ text strengthLine
    code >>> attr "style" "word-break: break-all;" $ textAt @"password"
    button @"Generate" # provided @"chosen" characterKinds
    indeterminateLinearProgress # action @"Generate" samplePassword
    capture @"Password generated"
  # looped # with strongMixRecipe
