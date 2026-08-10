module SignupFormMDC3 (signupFormMDC3) where

import Prelude (Unit, identity, (#), ($))

import Data.Profunctor.Row.RecordToRecord as RecordToRecord
import Data.Profunctor.Row.VariantToRecord as VariantToRecord
import Effect (Effect)
import PUI (asField, displayed, forCase, forField, mvu, required, toCases)
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
            debouncedTextField { floatingLabel: "Username", ms: usernameSettleTime } # asField @"value" @"username"
            radioButton
              [ { value: .free {}, label: "Free plan" }
              , { value: .pro {}, label: "Pro plan" }
              , { value: .team {}, label: "Team plan" }
              ] # required @"value" # asField @"value" @"plan"
            select { floatingLabel: "Country" }
              [ { value: .poland {}, label: "Poland" }
              , { value: .germany {}, label: "Germany" }
              , { value: .france {}, label: "France" }
              , { value: .spain {}, label: "Spain" }
              ] # required @"value" # asField @"value" @"country"
            filledTextField { floatingLabel: "Email" } # asField @"value" @"email"
            tooltip { text: "You must accept the terms of service to sign up" } $
              checkbox { ticked: {} } (staticText "I accept the terms of service") # asField @"value" @"terms") # mvu newApplicant
        ( bodyMedium $ staticText "Pick a username to check its availability" ) # providedCase @"unnamed" usernameStatus # displayed
        ( bodyMedium $ RecordToRecord.do
            staticText "✗ "
            text # forField @"value" @"username" identity
            staticText " is already taken" ) # providedCase @"taken" usernameStatus # displayed
        ( bodyMedium $ RecordToRecord.do
            staticText "✓ "
            text # forField @"value" @"username" identity
            staticText " is available" ) # providedCase @"available" usernameStatus # displayed
        ( titleSmall $ RecordToRecord.do
            staticText "⚠ "
            text # forField @"value" @"problem" identity ) # providedCase @"invalid" validation # displayed
        ( titleSmall $ RecordToRecord.do
            staticText "Ready to sign up as "
            text # forField @"value" @"username" identity ) # providedCase @"ready" validation # displayed
        button { label: "Sign up", icon: "person_add" } # toCases @"clicked" register
        VariantToRecord.do
          snackbar # forCase @"event" @"registered" welcomeLine
          snackbar # forCase @"event" @"rejected" rejectionLine
