module SignupFormMDC2 (signupFormMDC2) where

import Prelude (Unit, identity, (#), ($))

import Data.Profunctor.Row.RecordToRecord as RecordToRecord
import Data.Profunctor.Row.VariantToRecord as VariantToRecord
import Effect (Effect)
import PUI (PUI, asField, displayed, forCase, forField, mvu, required, toCases)
import PUI.Web.HTML (atCase, body, staticText, text)
import PUI.Web (Web)
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
            debouncedTextField { floatingLabel: "Username", ms: usernameSettleTime } # asField @"username"
            radioButton
              [ { value: .free {}, label: "Free plan" }
              , { value: .pro {}, label: "Pro plan" }
              , { value: .team {}, label: "Team plan" }
              ] # required # asField @"plan"
            select { floatingLabel: "Country" }
              [ { value: .poland {}, label: "Poland" }
              , { value: .germany {}, label: "Germany" }
              , { value: .france {}, label: "France" }
              , { value: .spain {}, label: "Spain" }
              ] # required # asField @"country"
            filledTextField { floatingLabel: "Email" } # asField @"email"
            tooltip { text: "You must accept the terms of service to sign up" } $
              checkbox (staticText "I accept the terms of service") # asField @"terms") # mvu newApplicant
        ( body2 $ staticText "Pick a username to check its availability" ) # atCase @"unnamed" usernameStatus # displayed
        ( body2 $ RecordToRecord.do
            staticText "✗ "
            text # forField @"username" identity
            staticText " is already taken" ) # atCase @"taken" usernameStatus # displayed
        ( body2 $ RecordToRecord.do
            staticText "✓ "
            text # forField @"username" identity
            staticText " is available" ) # atCase @"available" usernameStatus # displayed
        ( subtitle2 $ RecordToRecord.do
            staticText "⚠ "
            text # forField @"problem" identity ) # atCase @"invalid" validation # displayed
        ( subtitle2 $ RecordToRecord.do
            staticText "Ready to sign up as "
            text # forField @"username" identity ) # atCase @"ready" validation # displayed
        button { label: "Sign up", icon: "person_add" } # toCases register
        VariantToRecord.do
          welcomeToast
          rejectionToast

welcomeToast :: PUI Web [ registered :: String ] {}
welcomeToast = snackbar # forCase @"registered" welcomeLine

rejectionToast :: PUI Web [ rejected :: [ unnamed :: {}, taken :: { username :: String }, badEmail :: {}, termsUnaccepted :: {} ] ] {}
rejectionToast = snackbar # forCase @"rejected" rejectionLine
