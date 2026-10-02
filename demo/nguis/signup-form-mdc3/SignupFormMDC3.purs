module SignupFormMDC3 (signupFormMDC3) where

import Prelude (Unit, (#), ($))

import Effect (Effect)
import PUI (armed, mvu, state)
import PUI.Web ((<+>), choice, shown, shownWhen, staticText, text)
import PUI.Web.MDC3 (body, bodyMedium, button, checkbox, debouncedTextField, filledTextField, headlineLarge, radioButton, select, snackbar, titleSmall, tooltip)
import QualifiedDo.Semigroupoid as Semigroupoid
import SignupFormViewModel (availableLine, invalidLine, newApplicant, readyLine, signupLine, takenLine, unnamedLine, usernameSettleTime, usernameStatus, validation)

signupFormMDC3 :: Effect Unit
signupFormMDC3 =
  body $ Semigroupoid.do
    ( Semigroupoid.do
      (headlineLarge $ staticText @"Create account") # shown
      debouncedTextField @"Username" {} usernameSettleTime
      radioButton @"Plan"
        (choice @"Free" <+> choice @"Pro" <+> choice @"Team")
      select @"Country" {}
        (choice @"Poland" <+> choice @"Germany" <+> choice @"France" <+> choice @"Spain")
      filledTextField @"Email" {}
      checkbox @"Terms" @"accepted" @"declined" {} (staticText @"I accept the terms of service") # tooltip @"You must accept the terms of service to sign up"
    ) # mvu newApplicant
    ( bodyMedium $ text unnamedLine ) # shownWhen @"unnamed" usernameStatus
    ( Semigroupoid.do
      state @"Username" @String
      bodyMedium $ text takenLine ) # shownWhen @"taken" usernameStatus
    ( Semigroupoid.do
      state @"Username" @String
      bodyMedium $ text availableLine ) # shownWhen @"available" usernameStatus
    ( Semigroupoid.do
      state @"reason" @[ unnamed :: {}, taken :: { "Username" :: String }, badEmail :: {}, termsUnaccepted :: {} ]
      titleSmall $ text invalidLine ) # shownWhen @"invalid" validation
    ( Semigroupoid.do
      state @"Username" @String
      titleSmall $ text readyLine ) # shownWhen @"ready" validation
    button @"Sign up" { icon: "person_add" } # armed
    snackbar @"Sign up" signupLine
