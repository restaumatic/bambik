module SignupFormMDC3 (signupFormMDC3) where

import Prelude (Unit, (#), ($))

import Effect (Effect)
import PUI (armed, mvu)
import PUI.Web (choice, shown, shownWhen, staticText, text)
import PUI.Web.MDC3 (body, bodyMedium, button, checkbox, debouncedTextField, filledTextField, headlineLarge, radioButton, select, snackbar, titleSmall, tooltip)
import QualifiedDo.Semigroupoid as Semigroupoid
import SignupFormLogic (availableLine, invalidLine, newApplicant, readyLine, signupLine, takenLine, unnamedLine, usernameSettleTime, usernameStatus, validation)

signupFormMDC3 :: Effect Unit
signupFormMDC3 =
  body $ Semigroupoid.do
    ( Semigroupoid.do
      (headlineLarge $ staticText @"Create account") # shown
      debouncedTextField @"Username" {} usernameSettleTime
      radioButton @"Plan"
        [ choice @"Free", choice @"Pro", choice @"Team" ]
      select @"Country" {}
        [ choice @"Poland", choice @"Germany", choice @"France", choice @"Spain" ]
      filledTextField @"Email" {}
      checkbox @"Terms" @"accepted" @"declined" {} (staticText @"I accept the terms of service") # tooltip "You must accept the terms of service to sign up"
    ) # mvu newApplicant
    ( bodyMedium $ text unnamedLine ) # shownWhen @"unnamed" usernameStatus
    ( bodyMedium $ text takenLine ) # shownWhen @"taken" usernameStatus
    ( bodyMedium $ text availableLine ) # shownWhen @"available" usernameStatus
    ( titleSmall $ text invalidLine ) # shownWhen @"invalid" validation
    ( titleSmall $ text readyLine ) # shownWhen @"ready" validation
    button @"Sign up" { icon: "person_add" } # armed
    snackbar @"Sign up" signupLine
