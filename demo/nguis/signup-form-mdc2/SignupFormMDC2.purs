module SignupFormMDC2 (signupFormMDC2) where

import Prelude (Unit, (#), ($))

import Effect (Effect)
import PUI (armed, mvu)
import PUI.Web (choice, shown, shownWhen, staticText, text)
import PUI.Web.MDC2 (body, body2, button, card, checkbox, debouncedTextField, filledTextField, headline4, radioButton, select, snackbar, subtitle2, tooltip)
import QualifiedDo.Semigroupoid as Semigroupoid
import SignupFormLogic (availableLine, invalidLine, newApplicant, readyLine, signupLine, takenLine, usernameSettleTime, usernameStatus, validation)

signupFormMDC2 :: Effect Unit
signupFormMDC2 =
  body $
    card $ Semigroupoid.do
      ( Semigroupoid.do
        (headline4 $ staticText "Create account") # shown
        debouncedTextField @"Username" { ms: usernameSettleTime }
        radioButton @"Plan"
          [ choice @"Free", choice @"Pro", choice @"Team" ]
        select @"Country" {}
          [ choice @"Poland", choice @"Germany", choice @"France", choice @"Spain" ]
        filledTextField @"Email" {}
        checkbox @"Terms" @"accepted" @"declined" {} (staticText "I accept the terms of service") # tooltip "You must accept the terms of service to sign up"
      ) # mvu newApplicant
      ( body2 $ staticText "Pick a username to check its availability" ) # shownWhen @"unnamed" usernameStatus
      ( body2 $ text takenLine ) # shownWhen @"taken" usernameStatus
      ( body2 $ text availableLine ) # shownWhen @"available" usernameStatus
      ( subtitle2 $ text invalidLine ) # shownWhen @"invalid" validation
      ( subtitle2 $ text readyLine ) # shownWhen @"ready" validation
      button @"Sign up" { icon: "person_add" } # armed
      snackbar @"Sign up" signupLine
