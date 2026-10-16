module SignupForm.MDC3 (signupForm) where

import Prelude ((#), ($), identity, Unit)

import Effect (Effect)
import PUI (action, looped, with)
import PUI.Web ((<+>), choice, inCase, provided, staticText, text)
import PUI.Web.MDC3 (body, bodyMedium, button', checkbox, debouncedTextField, filledTextField, headlineLarge, indeterminateLinearProgress, radioButton, select, snackbar, titleSmall, tooltip)
import QualifiedDo.Semigroupoid as Semigroupoid
import SignupForm (availableLine, invalidLine, newApplicant, readyLine, signUp, takenLine, usernameSettleTime, usernameStatus, validation)

signupForm :: Effect Unit
signupForm =
  body $ Semigroupoid.do
    headlineLarge $ staticText "Create account"
    debouncedTextField @"Username" usernameSettleTime
    radioButton @"Plan"
      (choice @"Free" <+> choice @"Pro" <+> choice @"Team")
    select @"Country"
      (choice @"Poland" <+> choice @"Germany" <+> choice @"France" <+> choice @"Spain")
    filledTextField @"Email"
    checkbox @"Accept the terms of service" @"accepted" @"declined" # tooltip "You must accept the terms of service to sign up"
    ( bodyMedium $ staticText "Pick a username to check its availability" ) # inCase @"unnamed" usernameStatus
    ( bodyMedium $ text takenLine ) # inCase @"taken" usernameStatus
    ( bodyMedium $ text availableLine ) # inCase @"available" usernameStatus
    ( titleSmall $ text invalidLine ) # inCase @"invalid" validation
    ( titleSmall $ text readyLine ) # inCase @"ready" validation
    button' @"Sign up" { icon: "person_add" } # provided @"ready" validation
    indeterminateLinearProgress # action @"Sign up" signUp
    snackbar @"Account created" identity
  # looped # with newApplicant
