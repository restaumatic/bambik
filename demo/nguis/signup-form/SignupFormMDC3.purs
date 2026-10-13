module SignupFormMDC3 (signupFormMDC3) where

import Prelude (Unit, (#), ($))

import Data.Profunctor.Row.RecordToRecord as RecordToRecord
import Effect (Effect)
import PUI (looped, prefixed, with)
import PUI.Web ((<+>), choice, pane, staticText, text)
import PUI.Web.MDC3 (body, bodyMedium, button', checkbox, debouncedTextField, filledTextField, headlineLarge, radioButton, select, snackbar, titleSmall, tooltip)
import QualifiedDo.Semigroupoid as Semigroupoid
import SignupFormViewModel (availableLine, invalidLine, newApplicant, readyLine, signupLine, takenLine, unnamedLine, usernameSettleTime, usernameStatus, validation)

signupFormMDC3 :: Effect Unit
signupFormMDC3 =
  body $ Semigroupoid.do
    ( Semigroupoid.do
      prefixed (headlineLarge $ staticText "Create account") $ debouncedTextField @"Username" usernameSettleTime
      radioButton @"Plan"
        (choice @"Free" <+> choice @"Pro" <+> choice @"Team")
      select @"Country"
        (choice @"Poland" <+> choice @"Germany" <+> choice @"France" <+> choice @"Spain")
      filledTextField @"Email"
      checkbox @"I accept the terms of service" @"accepted" @"declined" # tooltip "You must accept the terms of service to sign up"
    ) # looped
      @( "Username" :: String
       , "Email" :: String
       , "Plan" :: [ "Free" :: {}, "Pro" :: {}, "Team" :: {} ]
       , "Country" :: [ "Poland" :: {}, "Germany" :: {}, "France" :: {}, "Spain" :: {} ]
       , "I accept the terms of service" :: [ accepted :: {}, declined :: {} ]
       ) # with newApplicant
    prefixed ( RecordToRecord.do
      ( bodyMedium $ text unnamedLine ) # pane @"unnamed"
        @( unnamed :: {}
         , taken :: { "Username" :: String }
         , available :: { "Username" :: String }
         ) usernameStatus
      ( bodyMedium $ text takenLine ) # pane @"taken" usernameStatus
      ( bodyMedium $ text availableLine ) # pane @"available" usernameStatus
      ( titleSmall $ text invalidLine ) # pane @"invalid"
        @( invalid :: { reason :: [ unnamed :: {}, taken :: { "Username" :: String }, badEmail :: {}, termsUnaccepted :: {} ] }
         , ready :: { "Username" :: String }
         ) validation
      ( titleSmall $ text readyLine ) # pane @"ready" validation ) $ button' @"Sign up" { icon: "person_add" }
    snackbar @"Sign up" signupLine
