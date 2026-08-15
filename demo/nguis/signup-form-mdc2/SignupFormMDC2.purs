module SignupFormMDC2 (signupFormMDC2) where

import Prelude (Unit, identity, (#), ($))

import Data.Profunctor.Row.RecordToRecord as RecordToRecord
import Data.Profunctor.Row.VariantToRecord as VariantToRecord
import Effect (Effect)
import PUI (displayed, forCase, mvu, required, toCases)
import PUI.Web (choice)
import Data.Tuple.Nested ((/\))
import PUI.Web.HTML (providedCase, body, staticText, text)
import PUI.Web.MDC2 (body2, button, card, checkbox, debouncedTextField, elevation20, filledTextField, headline4, radioButton, select, snackbar, subtitle2, tooltip)
import QualifiedDo.Semigroupoid as Semigroupoid
import SignupFormLogic (newApplicant, register, rejectionLine, usernameSettleTime, usernameStatus, validation, welcomeLine)

signupFormMDC2 :: Effect Unit
signupFormMDC2 =
  body $
    elevation20 $
      card { caption: "Sign-Up Form" } $ Semigroupoid.do
        ( RecordToRecord.do
            headline4 $ staticText "Create account"
            debouncedTextField @"Username" { ms: usernameSettleTime }
            radioButton @"Plan"
              [ choice @"Free plan", choice @"Pro plan", choice @"Team plan" ] # required
            select @"Country" {}
              [ choice @"Poland", choice @"Germany", choice @"France", choice @"Spain" ] # required
            filledTextField @"Email" {}
            tooltip { text: "You must accept the terms of service to sign up" } $
              checkbox @"Terms" { ticked: {} } (staticText "I accept the terms of service")) # mvu newApplicant
        ( body2 $ staticText "Pick a username to check its availability" ) # providedCase @"unnamed" usernameStatus # displayed
        ( body2 $ RecordToRecord.do
            staticText "✗ "
            text @"Username"
            staticText " is already taken" ) # providedCase @"taken" usernameStatus # displayed
        ( body2 $ RecordToRecord.do
            staticText "✓ "
            text @"Username"
            staticText " is available" ) # providedCase @"available" usernameStatus # displayed
        ( subtitle2 $ RecordToRecord.do
            staticText "⚠ "
            text @"problem" ) # providedCase @"invalid" validation # displayed
        ( subtitle2 $ RecordToRecord.do
            staticText "Ready to sign up as "
            text @"Username" ) # providedCase @"ready" validation # displayed
        button @"Sign up" { icon: "person_add" } # toCases register
        VariantToRecord.do
          snackbar # forCase @"registered" welcomeLine
          snackbar # forCase @"rejected" rejectionLine
