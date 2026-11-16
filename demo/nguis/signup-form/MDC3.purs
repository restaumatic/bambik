module SignupForm.MDC3 (signupForm) where

import Prelude ((#), ($), Unit)

import Data.Profunctor.Row.VariantToVariant as VariantToVariant
import Effect (Effect)
import PUI (action, captured, introduced)
import PUI.Web (inCase, provided, staticText, text)
import PUI.Web.MDC3 (body, bodyMedium, button', checkbox, debouncedTextField, filledTextField, headlineLarge, indeterminateLinearProgress, option, radio, radioButton, select, snackbar, titleSmall, tooltip)
import QualifiedDo.Semigroupoid as Semigroupoid
import SignupForm (availableLine, blankEmail, blankUsername, freePlan, homeCountry, invalidLine, readyLine, signUp, termsDeclined, usernameSettleTime, usernameStatus, validation)

signupForm :: Effect Unit
signupForm =
  body $ Semigroupoid.do
    headlineLarge $ staticText "Create account"
    debouncedTextField @"Username" usernameSettleTime # introduced @"Username" blankUsername
    ( radioButton @"Plan" $ VariantToVariant.do
      radio @"Free"
      radio @"Pro"
      radio @"Team" ) # introduced @"Plan" freePlan
    ( select @"Country" $ VariantToVariant.do
      option @"Poland"
      option @"Germany"
      option @"France"
      option @"Spain" ) # introduced @"Country" homeCountry
    filledTextField @"Email" # introduced @"Email" blankEmail
    checkbox @"Accept the terms of service" @"accepted" @"declined" # introduced @"Accept the terms of service" termsDeclined # tooltip "You must accept the terms of service to sign up"
    ( bodyMedium $ text availableLine ) # inCase @"available" usernameStatus
    ( titleSmall $ text invalidLine ) # inCase @"invalid" validation
    ( titleSmall $ text readyLine ) # inCase @"ready" validation
    button' @"Sign up" { icon: "person_add" } # provided @"ready" validation
    indeterminateLinearProgress # action @"Sign up" signUp
    snackbar @"Account created" captured
