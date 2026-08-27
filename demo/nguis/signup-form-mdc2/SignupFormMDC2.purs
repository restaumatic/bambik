module SignupFormMDC2 (signupFormMDC2) where

import Prelude (Unit, (#), ($))

import Data.Profunctor.Row.RecordToRecord as RecordToRecord
import Data.Profunctor.Row.VariantToRecord as VariantToRecord
import Effect (Effect)
import PUI (forCase, mvu, required, toCases)
import PUI.Web (choice)
import PUI.Web.HTML (shown, shownWhen, body, staticText, text)
import PUI.Web.MDC2 (body2, button, card, checkbox, debouncedTextField, elevation20, filledTextField, headline4, radioButton, select, snackbar, subtitle2, tooltip)
import QualifiedDo.Category as Category
import SignupFormLogic (newApplicant, register, rejectionLine, usernameSettleTime, usernameStatus, validation, welcomeLine)

signupFormMDC2 :: Effect Unit
signupFormMDC2 =
  body $
    elevation20 $
      card $ Category.do
        ( Category.do
            (headline4 $ staticText "Create account") # shown
            debouncedTextField @"Username" { ms: usernameSettleTime }
            radioButton @"Plan"
              [ choice @"Free", choice @"Pro", choice @"Team" ] # required
            select @"Country" {}
              [ choice @"Poland", choice @"Germany", choice @"France", choice @"Spain" ] # required
            filledTextField @"Email" {}
            tooltip { text: "You must accept the terms of service to sign up" } $
              checkbox @"Terms" @"accepted" @"declined" { ticked: {} } (staticText "I accept the terms of service")) # mvu newApplicant
        ( body2 $ staticText "Pick a username to check its availability" ) # shownWhen @"unnamed" usernameStatus
        ( body2 $ RecordToRecord.do
            staticText "✗ "
            text @"Username"
            staticText " is already taken" ) # shownWhen @"taken" usernameStatus
        ( body2 $ RecordToRecord.do
            staticText "✓ "
            text @"Username"
            staticText " is available" ) # shownWhen @"available" usernameStatus
        ( subtitle2 $ RecordToRecord.do
            staticText "⚠ "
            text @"problem" ) # shownWhen @"invalid" validation
        ( subtitle2 $ RecordToRecord.do
            staticText "Ready to sign up as "
            text @"Username" ) # shownWhen @"ready" validation
        button @"Sign up" { icon: "person_add" } # toCases register
        VariantToRecord.do
          snackbar # forCase @"registered" welcomeLine
          snackbar # forCase @"rejected" rejectionLine
