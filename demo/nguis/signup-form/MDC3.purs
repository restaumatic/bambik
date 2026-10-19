module SignupForm.MDC3 (signupForm) where

import Prelude ((#), ($), identity, Unit)

import Data.Profunctor.Row.VariantToVariant as VariantToVariant
import Effect (Effect)
import PUI (action)
import PUI.Web (inCase, provided, staticText, text)
import PUI.Web.MDC3 (body, bodyMedium, button', checkboxFrom, debouncedTextFieldFrom, filledTextFieldFrom, headlineLarge, indeterminateLinearProgress, option, radio, radioButtonFrom, selectFrom, snackbar, titleSmall, tooltip)
import QualifiedDo.Semigroupoid as Semigroupoid
import SignupForm (availableLine, blankEmail, blankUsername, defaultCountry, defaultPlan, invalidLine, readyLine, signUp, takenLine, termsUndecided, usernameSettleTime, usernameStatus, validation)

signupForm :: Effect Unit
signupForm =
  body $ Semigroupoid.do
    headlineLarge $ staticText "Create account"
    debouncedTextFieldFrom @"Username" blankUsername usernameSettleTime
    radioButtonFrom @"Plan" defaultPlan $ VariantToVariant.do
      radio @"Free"
      radio @"Pro"
      radio @"Team"
    selectFrom @"Country" defaultCountry $ VariantToVariant.do
      option @"Poland"
      option @"Germany"
      option @"France"
      option @"Spain"
    filledTextFieldFrom @"Email" blankEmail
    checkboxFrom @"Accept the terms of service" @"accepted" @"declined" termsUndecided # tooltip "You must accept the terms of service to sign up"
    ( bodyMedium $ staticText "Pick a username to check its availability" ) # inCase @"unnamed" usernameStatus
    ( bodyMedium $ text takenLine ) # inCase @"taken" usernameStatus
    ( bodyMedium $ text availableLine ) # inCase @"available" usernameStatus
    ( titleSmall $ text invalidLine ) # inCase @"invalid" validation
    ( titleSmall $ text readyLine ) # inCase @"ready" validation
    button' @"Sign up" { icon: "person_add" } # provided @"ready" validation
    indeterminateLinearProgress # action @"Sign up" signUp
    snackbar @"Account created" identity
