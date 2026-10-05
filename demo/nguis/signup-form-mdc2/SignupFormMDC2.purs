module SignupFormMDC2 (signupFormMDC2) where

import Prelude (Unit, (#), ($))

import Effect (Effect)
import PUI (armed, looped, with)
import PUI.Web ((<+>), choice, shown, shownWhen, staticText, text)
import PUI.Web.MDC2 (body, body2, button, checkbox, debouncedTextField, filledTextField, headline4, radioButton, select, snackbar, subtitle2, tooltip)
import QualifiedDo.Semigroupoid as Semigroupoid
import SignupFormViewModel (availableLine, invalidLine, newApplicant, readyLine, signupLine, takenLine, unnamedLine, usernameSettleTime, usernameStatus, validation)

signupFormMDC2 :: Effect Unit
signupFormMDC2 =
  body $ Semigroupoid.do
    ( Semigroupoid.do
      (headline4 $ staticText @"Create account") # shown
      debouncedTextField @"Username" {} usernameSettleTime
      radioButton @"Plan"
        (choice @"Free" <+> choice @"Pro" <+> choice @"Team")
      select @"Country" {}
        (choice @"Poland" <+> choice @"Germany" <+> choice @"France" <+> choice @"Spain")
      filledTextField @"Email" {}
      checkbox @"Terms" @"accepted" @"declined" {} (staticText @"I accept the terms of service") # tooltip @"You must accept the terms of service to sign up"
    ) # looped
      @( "Username" :: String
       , "Email" :: String
       , "Plan" :: [ "Free" :: {}, "Pro" :: {}, "Team" :: {} ]
       , "Country" :: [ "Poland" :: {}, "Germany" :: {}, "France" :: {}, "Spain" :: {} ]
       , "Terms" :: [ accepted :: {}, declined :: {} ]
       ) # with newApplicant
    ( body2 $ text unnamedLine ) # shownWhen @"unnamed" @( unnamed :: {}, taken :: { "Username" :: String }, available :: { "Username" :: String } ) usernameStatus
    ( body2 $ text takenLine ) # shownWhen @"taken" usernameStatus
    ( body2 $ text availableLine ) # shownWhen @"available" usernameStatus
    ( subtitle2 $ text invalidLine ) # shownWhen @"invalid" @( invalid :: { reason :: [ unnamed :: {}, taken :: { "Username" :: String }, badEmail :: {}, termsUnaccepted :: {} ] }, ready :: { "Username" :: String } ) validation
    ( subtitle2 $ text readyLine ) # shownWhen @"ready" validation
    button @"Sign up" { icon: "person_add" } # armed
    snackbar @"Sign up" signupLine
