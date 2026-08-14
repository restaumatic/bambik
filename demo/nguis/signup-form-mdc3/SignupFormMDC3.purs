module SignupFormMDC3 (signupFormMDC3) where

import Prelude (Unit, identity, (#), ($))

import Data.Profunctor.Row.RecordToRecord as RecordToRecord
import Data.Profunctor.Row.VariantToRecord as VariantToRecord
import Effect (Effect)
import PUI (displayed, forCase, mvu, required, toCases)
import PUI.Web.HTML (providedCase, body, staticText, text)
import PUI.Web.MDC3 (bodyMedium, button, card, checkbox, debouncedTextField, elevation5, filledTextField, headlineLarge, radioButton, select, snackbar, titleSmall, tooltip)
import QualifiedDo.Semigroupoid as Semigroupoid
import SignupFormLogic (newApplicant, register, rejectionLine, usernameSettleTime, usernameStatus, validation, welcomeLine)

signupFormMDC3 :: Effect Unit
signupFormMDC3 =
  body $
    elevation5 $
      card { caption: "Sign-Up Form" } $ Semigroupoid.do
        ( RecordToRecord.do
            headlineLarge $ staticText "Create account"
            debouncedTextField @"Username" { ms: usernameSettleTime }
            radioButton @"Plan"
              [ { value: .free {}, label: "Free plan" }
              , { value: .pro {}, label: "Pro plan" }
              , { value: .team {}, label: "Team plan" }
              ] # required
            select @"Country" {}
              [ { value: .poland {}, label: "Poland" }
              , { value: .germany {}, label: "Germany" }
              , { value: .france {}, label: "France" }
              , { value: .spain {}, label: "Spain" }
              ] # required
            filledTextField @"Email" {}
            tooltip { text: "You must accept the terms of service to sign up" } $
              checkbox @"Terms" { ticked: {} } (staticText "I accept the terms of service")) # mvu newApplicant
        ( bodyMedium $ staticText "Pick a username to check its availability" ) # providedCase @"unnamed" usernameStatus # displayed
        ( bodyMedium $ RecordToRecord.do
            staticText "✗ "
            text @"Username"
            staticText " is already taken" ) # providedCase @"taken" usernameStatus # displayed
        ( bodyMedium $ RecordToRecord.do
            staticText "✓ "
            text @"Username"
            staticText " is available" ) # providedCase @"available" usernameStatus # displayed
        ( titleSmall $ RecordToRecord.do
            staticText "⚠ "
            text @"problem" ) # providedCase @"invalid" validation # displayed
        ( titleSmall $ RecordToRecord.do
            staticText "Ready to sign up as "
            text @"Username" ) # providedCase @"ready" validation # displayed
        button @"Sign up" { icon: "person_add" } # toCases register
        VariantToRecord.do
          snackbar # forCase @"registered" welcomeLine
          snackbar # forCase @"rejected" rejectionLine
