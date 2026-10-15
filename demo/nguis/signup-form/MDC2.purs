module SignupForm.MDC2 (signupForm) where

import Prelude ((#), ($), identity, Unit)

import Effect (Effect)
import PUI (action, looped, with)
import PUI.Web ((<+>), choice, inCase, provided, staticText, text)
import PUI.Web.MDC2 (body, body2, button', checkbox, debouncedTextField, filledTextField, headline4, indeterminateLinearProgress, radioButton, select, snackbar, subtitle2, tooltip)
import QualifiedDo.Semigroupoid as Semigroupoid
import SignupForm (availableLine, invalidLine, newApplicant, readyLine, signUp, takenLine, usernameSettleTime, usernameStatus, validation)

signupForm :: Effect Unit
signupForm =
  body $ Semigroupoid.do
    headline4 $ staticText "Create account"
    debouncedTextField @"Username" usernameSettleTime
    radioButton @"Plan"
      (choice @"Free" <+> choice @"Pro" <+> choice @"Team")
    select @"Country"
      (choice @"Poland" <+> choice @"Germany" <+> choice @"France" <+> choice @"Spain")
    filledTextField @"Email"
    checkbox @"I accept the terms of service" @"accepted" @"declined" # tooltip "You must accept the terms of service to sign up"
    ( body2 $ staticText "Pick a username to check its availability" ) # inCase @"unnamed" usernameStatus
    ( body2 $ text takenLine ) # inCase @"taken" usernameStatus
    ( body2 $ text availableLine ) # inCase @"available" usernameStatus
    ( subtitle2 $ text invalidLine ) # inCase @"invalid" validation
    ( subtitle2 $ text readyLine ) # inCase @"ready" validation
    button' @"Sign up" { icon: "person_add" } # provided @"ready" validation
    indeterminateLinearProgress # action @"Sign up" @( "Account created" :: _ ) signUp
    snackbar @"Account created" identity
  # looped # with newApplicant
