module SignupForm.MDC2 (signupForm) where

import Prelude ((#), ($), identity, Unit)

import Data.Profunctor.Row.VariantToVariant as VariantToVariant
import Effect (Effect)
import PUI (action, looped, with)
import PUI.Web (inCase, provided, staticText, text)
import PUI.Web.MDC2 (body, body2, button', checkbox, debouncedTextField, filledTextField, headline4, indeterminateLinearProgress, option, radio, radioButton, select, snackbar, subtitle2, tooltip)
import QualifiedDo.Semigroupoid as Semigroupoid
import SignupForm (availableLine, invalidLine, newApplicant, readyLine, signUp, usernameSettleTime, usernameStatus, validation)

signupForm :: Effect Unit
signupForm =
  body $ Semigroupoid.do
    headline4 $ staticText "Create account"
    ( Semigroupoid.do
      debouncedTextField @"Username" usernameSettleTime
      radioButton @"Plan" $ VariantToVariant.do
        radio @"Free"
        radio @"Pro"
        radio @"Team"
      select @"Country" $ VariantToVariant.do
        option @"Poland"
        option @"Germany"
        option @"France"
        option @"Spain"
      filledTextField @"Email"
      checkbox @"Accept the terms of service" @"accepted" @"declined" # tooltip "You must accept the terms of service to sign up"
    ) # looped # with newApplicant
    ( body2 $ text availableLine ) # inCase @"available" usernameStatus
    ( subtitle2 $ text invalidLine ) # inCase @"invalid" validation
    ( subtitle2 $ text readyLine ) # inCase @"ready" validation
    button' @"Sign up" { icon: "person_add" } # provided @"ready" validation
    indeterminateLinearProgress # action @"Sign up" signUp
    snackbar @"Account created" identity
