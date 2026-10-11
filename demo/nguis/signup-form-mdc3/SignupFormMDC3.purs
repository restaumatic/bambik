module SignupFormMDC3 (signupFormMDC3) where

import Prelude (Unit, (#), ($))

import Effect (Effect)
import PUI (armed, looped, with)
import PUI.Pipeline as Pipeline
import PUI.Web ((<+>), choice, pane, staticText, text)
import PUI.Web.MDC3 (body, bodyMedium, button, checkbox, debouncedTextField, filledTextField, headlineLarge, radioButton, select, snackbar, titleSmall, tooltip)
import SignupFormViewModel (availableLine, invalidLine, newApplicant, readyLine, signupLine, takenLine, unnamedLine, usernameSettleTime, usernameStatus, validation)

signupFormMDC3 :: Effect Unit
signupFormMDC3 =
  body $ Pipeline.do
    ( Pipeline.do
      headlineLarge $ staticText "Create account"
      debouncedTextField @"Username" {} usernameSettleTime
      radioButton @"Plan"
        (choice @"Free" <+> choice @"Pro" <+> choice @"Team")
      select @"Country" {}
        (choice @"Poland" <+> choice @"Germany" <+> choice @"France" <+> choice @"Spain")
      filledTextField @"Email" {}
      checkbox @"Terms" @"accepted" @"declined" {} (staticText "I accept the terms of service") # tooltip "You must accept the terms of service to sign up"
    ) # looped
      @( "Username" :: String
       , "Email" :: String
       , "Plan" :: [ "Free" :: {}, "Pro" :: {}, "Team" :: {} ]
       , "Country" :: [ "Poland" :: {}, "Germany" :: {}, "France" :: {}, "Spain" :: {} ]
       , "Terms" :: [ accepted :: {}, declined :: {} ]
       ) # with newApplicant
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
    ( titleSmall $ text readyLine ) # pane @"ready" validation
    button @"Sign up" { icon: "person_add" } # armed
    snackbar @"Sign up" signupLine
