module SignupFormMDC2 (signupFormMDC2) where

import Prelude (Unit, identity, (#), ($))

import Data.Profunctor.Row.RecordToRecord as RecordToRecord
import Data.Profunctor.Row.VariantToRecord as VariantToRecord
import Effect (Effect)
import PUI (displayed, forCase, mvu, required, toCases)
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
            debouncedTextField @"username" { ms: usernameSettleTime }
            radioButton @"plan"
              [ { value: .free {}, label: "Free plan" }
              , { value: .pro {}, label: "Pro plan" }
              , { value: .team {}, label: "Team plan" }
              ] # required
            select @"country" {}
              [ { value: .poland {}, label: "Poland" }
              , { value: .germany {}, label: "Germany" }
              , { value: .france {}, label: "France" }
              , { value: .spain {}, label: "Spain" }
              ] # required
            filledTextField @"email" {}
            tooltip { text: "You must accept the terms of service to sign up" } $
              checkbox @"terms" { ticked: {} } (staticText "I accept the terms of service")) # mvu newApplicant
        ( body2 $ staticText "Pick a username to check its availability" ) # providedCase @"unnamed" usernameStatus # displayed
        ( body2 $ RecordToRecord.do
            staticText "✗ "
            text @"username"
            staticText " is already taken" ) # providedCase @"taken" usernameStatus # displayed
        ( body2 $ RecordToRecord.do
            staticText "✓ "
            text @"username"
            staticText " is available" ) # providedCase @"available" usernameStatus # displayed
        ( subtitle2 $ RecordToRecord.do
            staticText "⚠ "
            text @"problem" ) # providedCase @"invalid" validation # displayed
        ( subtitle2 $ RecordToRecord.do
            staticText "Ready to sign up as "
            text @"username" ) # providedCase @"ready" validation # displayed
        button { label: "Sign up", icon: "person_add" } # toCases @"clicked" register
        VariantToRecord.do
          snackbar # forCase @"event" @"registered" welcomeLine
          snackbar # forCase @"event" @"rejected" rejectionLine
