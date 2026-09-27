module SignupFormMDC3 (signupFormMDC3) where

import Prelude (Unit, (#), ($))

import Effect (Effect)
import PUI (armed, mvu)
import PUI.Web (choice, shown, shownWhen, staticText, text)
import PUI.Web.MDC3 (body, bodyMedium, button, card, checkbox, debouncedTextField, filledTextField, headlineLarge, radioButton, select, snackbar, titleSmall, tooltip)
import QualifiedDo.Semigroupoid as Semigroupoid
import SignupFormLogic (availableLine, invalidLine, newApplicant, readyLine, signupLine, takenLine, usernameSettleTime, usernameStatus, validation)

signupFormMDC3 :: Effect Unit
signupFormMDC3 =
  body $
    card $ Semigroupoid.do
      ( Semigroupoid.do
        (headlineLarge $ staticText "Create account") # shown
        debouncedTextField @"Username" { ms: usernameSettleTime }
        radioButton @"Plan"
          [ choice @"Free", choice @"Pro", choice @"Team" ]
        select @"Country" {}
          [ choice @"Poland", choice @"Germany", choice @"France", choice @"Spain" ]
        filledTextField @"Email" {}
        checkbox @"Terms" @"accepted" @"declined" { ticked: {} } (staticText "I accept the terms of service") # tooltip { text: "You must accept the terms of service to sign up" }
      ) # mvu newApplicant
      ( bodyMedium $ staticText "Pick a username to check its availability" ) # shownWhen @"unnamed" usernameStatus
      ( bodyMedium $ text takenLine ) # shownWhen @"taken" usernameStatus
      ( bodyMedium $ text availableLine ) # shownWhen @"available" usernameStatus
      ( titleSmall $ text invalidLine ) # shownWhen @"invalid" validation
      ( titleSmall $ text readyLine ) # shownWhen @"ready" validation
      button @"Sign up" { icon: "person_add" } # armed
      snackbar { "Sign up": signupLine }
