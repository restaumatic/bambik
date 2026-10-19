module SignupForm.MDC3 (signupForm) where

import Prelude ((#), ($), identity, Unit)

import Data.Profunctor.Row.VariantToVariant as VariantToVariant
import Effect (Effect)
import PUI (action)
import PUI.Web (inCase, provided, staticText, text)
import PUI.Web.MDC3 (body, bodyMedium, button', checkboxFrom, debouncedTextFieldFrom, filledTextFieldFrom, headlineLarge, indeterminateLinearProgress, option, radio, radioButtonFrom, selectFrom, snackbar, titleSmall, tooltip)
import QualifiedDo.Semigroupoid as Semigroupoid
import SignupForm (availableLine, blankEmail, blankUsername, freePlan, homeCountry, invalidLine, readyLine, signUp, termsDeclined, usernameSettleTime, usernameStatus, validation)

signupForm :: Effect Unit
signupForm =
  body $ Semigroupoid.do
    headlineLarge $ staticText "Create account"
    debouncedTextFieldFrom @"Username" blankUsername usernameSettleTime
    radioButtonFrom @"Plan" freePlan $ VariantToVariant.do
      radio @"Free"
      radio @"Pro"
      radio @"Team"
    selectFrom @"Country" homeCountry $ VariantToVariant.do
      option @"Poland"
      option @"Germany"
      option @"France"
      option @"Spain"
    filledTextFieldFrom @"Email" blankEmail
    checkboxFrom @"Accept the terms of service" @"accepted" @"declined" termsDeclined # tooltip "You must accept the terms of service to sign up"
    ( bodyMedium $ text availableLine ) # inCase @"available" usernameStatus
    ( titleSmall $ text invalidLine ) # inCase @"invalid" validation
    ( titleSmall $ text readyLine ) # inCase @"ready" validation
    button' @"Sign up" { icon: "person_add" } # provided @"ready" validation
    indeterminateLinearProgress # action @"Sign up" signUp
    snackbar @"Account created" identity
