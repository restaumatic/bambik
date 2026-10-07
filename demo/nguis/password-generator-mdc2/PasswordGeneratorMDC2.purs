module PasswordGeneratorMDC2 (passwordGeneratorMDC2) where

import Prelude (identity, Unit, (#), ($), (>>>))

import Effect (Effect)
import PasswordGeneratorViewModel (passwordGeneratedLine, passwordText, samplePassword, strengthLine, strongMixRecipe)
import PUI (action, atCase, fold, looped, with)
import PUI.Web (attr, shown, text)
import PUI.Web.HTML (code)
import PUI.Web.MDC2 (body, body2, button, indeterminateLinearProgress, slider, snackbar, toggleSwitch)
import QualifiedDo.Semigroupoid as Semigroupoid

passwordGeneratorMDC2 :: Effect Unit
passwordGeneratorMDC2 =
  body $
    ( Semigroupoid.do
      slider @"Length" {}
      toggleSwitch @"Uppercase letters" {}
      toggleSwitch @"Lowercase letters" {}
      toggleSwitch @"Digits" {}
      toggleSwitch @"Symbols" {}
      body2 (text strengthLine) # shown
      code >>> attr "style" "word-break: break-all;" $ text passwordText # shown
      button @"Generate" {}
      indeterminateLinearProgress # action samplePassword # atCase @"Generate"
      snackbar @"Password generated" passwordGeneratedLine # fold identity
    ) # looped
      @( "Length" :: { current :: Number, min :: Number, max :: Number, step :: [ discrete :: Number, continuous :: {} ] }
       , "Uppercase letters" :: Boolean
       , "Lowercase letters" :: Boolean
       , "Digits" :: Boolean
       , "Symbols" :: Boolean
       , password :: String
       ) # with strongMixRecipe
