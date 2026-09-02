module SignupFormMDC3 (signupFormMDC3) where

import Prelude (Unit, (#), ($))

import Data.Profunctor.Row.VariantToRecord as VariantToRecord
import Effect (Effect)
import PUI (forCase, mvu, required, toCases)
import PUI.Web (choice)
import PUI.Web.HTML (shown, shownWhen, body, staticText, text)
import PUI.Web.MDC3 (bodyMedium, button, card, checkbox, debouncedTextField, elevation5, filledTextField, headlineLarge, radioButton, select, snackbar, titleSmall, tooltip)
import QualifiedDo.Category as Category
import SignupFormLogic (newApplicant, register, rejectionLine, usernameSettleTime, usernameStatus, validation, welcomeLine)

signupFormMDC3 :: Effect Unit
signupFormMDC3 =
  body $
    elevation5 $
      card $ Category.do
        ( Category.do
            (headlineLarge $ staticText "Create account") # shown
            debouncedTextField @"Username" { ms: usernameSettleTime }
            radioButton @"Plan"
              [ choice @"Free", choice @"Pro", choice @"Team" ] # required
            select @"Country" {}
              [ choice @"Poland", choice @"Germany", choice @"France", choice @"Spain" ] # required
            filledTextField @"Email" {}
            tooltip { text: "You must accept the terms of service to sign up" } $
              checkbox @"Terms" @"accepted" @"declined" { ticked: {} } (staticText "I accept the terms of service")) # mvu newApplicant
        ( bodyMedium $ staticText "Pick a username to check its availability" ) # shownWhen @"unnamed" usernameStatus
        ( bodyMedium $ text @"takenLine" ) # shownWhen @"taken" usernameStatus
        ( bodyMedium $ text @"availableLine" ) # shownWhen @"available" usernameStatus
        ( titleSmall $ text @"invalidLine" ) # shownWhen @"invalid" validation
        ( titleSmall $ text @"readyLine" ) # shownWhen @"ready" validation
        button @"Sign up" { icon: "person_add" } # toCases register
        VariantToRecord.do
          snackbar # forCase @"registered" welcomeLine
          snackbar # forCase @"rejected" rejectionLine
