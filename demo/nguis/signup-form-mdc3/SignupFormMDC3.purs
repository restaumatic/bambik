module SignupFormMDC3 (signupFormMDC3) where

import Prelude (Unit, identity, (#), ($))

import Data.Profunctor.Row.RecordToRecord as RecordToRecord
import Data.Profunctor.Row.VariantToRecord as VariantToRecord
import Effect (Effect)
import PUI (forCase, mvu, required, toCases)
import PUI.Web (choice)
import PUI.Web.HTML (shownAs, shownCase, body, staticText, text)
import PUI.Web.MDC3 (bodyMedium, button, card, checkbox, debouncedTextField, elevation5, filledTextField, headlineLarge, radioButton, select, snackbar, titleSmall, tooltip)
import QualifiedDo.Semigroupoid as Semigroupoid
import SignupFormLogic (newApplicant, register, rejectionLine, usernameSettleTime, usernameStatus, validation, welcomeLine)

signupFormMDC3 :: Effect Unit
signupFormMDC3 =
  body $
    elevation5 $
      card $ Semigroupoid.do
        ( Semigroupoid.do
            (headlineLarge $ staticText "Create account") # shownAs identity
            debouncedTextField @"Username" { ms: usernameSettleTime }
            radioButton @"Plan"
              [ choice @"Free", choice @"Pro", choice @"Team" ] # required
            select @"Country" {}
              [ choice @"Poland", choice @"Germany", choice @"France", choice @"Spain" ] # required
            filledTextField @"Email" {}
            tooltip { text: "You must accept the terms of service to sign up" } $
              checkbox @"Terms" { ticked: {} } (staticText "I accept the terms of service")) # mvu newApplicant
        ( bodyMedium $ staticText "Pick a username to check its availability" ) # shownCase @"unnamed" usernameStatus
        ( bodyMedium $ RecordToRecord.do
            staticText "✗ "
            text @"Username"
            staticText " is already taken" ) # shownCase @"taken" usernameStatus
        ( bodyMedium $ RecordToRecord.do
            staticText "✓ "
            text @"Username"
            staticText " is available" ) # shownCase @"available" usernameStatus
        ( titleSmall $ RecordToRecord.do
            staticText "⚠ "
            text @"problem" ) # shownCase @"invalid" validation
        ( titleSmall $ RecordToRecord.do
            staticText "Ready to sign up as "
            text @"Username" ) # shownCase @"ready" validation
        button @"Sign up" { icon: "person_add" } # toCases register
        VariantToRecord.do
          snackbar # forCase @"registered" welcomeLine
          snackbar # forCase @"rejected" rejectionLine
