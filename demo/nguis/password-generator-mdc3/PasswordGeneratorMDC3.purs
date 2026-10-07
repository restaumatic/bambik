module PasswordGeneratorMDC3 (passwordGeneratorMDC3) where

import Prelude (identity, Unit, (#), ($), (>>>))

import Data.Profunctor.Row.VariantToRecord as VariantToRecord
import Effect (Effect)
import PasswordGeneratorViewModel (passwordGeneratedLine, passwordText, samplePassword, strengthLine, strongMixRecipe)
import PUI (action, atCase, blankStatus, fold, looped, with)
import PUI.Web (attr, shown, text)
import PUI.Web.HTML (code)
import PUI.Web.MDC3 (body, bodyMedium, button, indeterminateLinearProgress, slider, snackbar, toggleSwitch)
import QualifiedDo.Semigroupoid as Semigroupoid

passwordGeneratorMDC3 :: Effect Unit
passwordGeneratorMDC3 =
  body $
    ( Semigroupoid.do
      slider @"Length" {}
      toggleSwitch @"Uppercase letters" {}
      toggleSwitch @"Lowercase letters" {}
      toggleSwitch @"Digits" {}
      toggleSwitch @"Symbols" {}
      bodyMedium (text strengthLine) # shown
      code >>> attr "style" "word-break: break-all;" $ text passwordText # shown
      button @"Generate" {}
      ( VariantToRecord.do
        indeterminateLinearProgress @"Generating password"
        blankStatus @"Password generated" ) # action samplePassword # atCase @"Generate"
      snackbar @"Password generated" passwordGeneratedLine # fold identity
    ) # looped
      @( "Length" :: { current :: Number, min :: Number, max :: Number, step :: [ discrete :: Number, continuous :: {} ] }
       , "Uppercase letters" :: Boolean
       , "Lowercase letters" :: Boolean
       , "Digits" :: Boolean
       , "Symbols" :: Boolean
       , password :: String
       ) # with strongMixRecipe
