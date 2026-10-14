module SignupForm.MDC3 (signupForm) where

import Prelude ((#), ($), identity, Unit)

import Effect (Effect)
import PUI (looped, with)
import PUI.Web ((<+>), choice, inCase, staticText, text)
import PUI.Web.MDC3 (body, bodyMedium, button', checkbox, debouncedTextField, filledTextField, headlineLarge, radioButton, select, snackbar, titleSmall, tooltip)
import QualifiedDo.Semigroupoid as Semigroupoid
import SignupForm (availableLine, invalidLine, newApplicant, readyLine, signupLine, takenLine, usernameSettleTime, usernameStatus, validation)

signupForm :: Effect Unit
signupForm =
  body $ Semigroupoid.do
    ( Semigroupoid.do
      headlineLarge $ staticText "Create account"
      debouncedTextField @"Username" usernameSettleTime
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
    ( bodyMedium $ staticText "Pick a username to check its availability" ) # inCase @"unnamed" @( unnamed :: {}, taken :: {}, available :: {} ) usernameStatus
    ( bodyMedium $ text takenLine ) # inCase @"taken" usernameStatus
    ( bodyMedium $ text availableLine ) # inCase @"available" usernameStatus
    ( titleSmall $ text invalidLine ) # inCase @"invalid" @( invalid :: {}, ready :: {} ) validation
    ( titleSmall $ text readyLine ) # inCase @"ready" validation
    button' @"Sign up" { icon: "person_add" }
    snackbar @"Sign up" signupLine identity
