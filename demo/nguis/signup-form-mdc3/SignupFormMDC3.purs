module SignupFormMDC3 (signupFormMDC3) where

import Prelude (Unit, identity, (#), ($), (<>), (==), (>>>))

import Data.Either (Either(..), either)
import Data.Foldable (elem)
import Data.Maybe (Maybe(..), isJust)
import Data.Profunctor.Row.RecordToRecord as RecordToRecord
import Data.Profunctor.Row.VariantToRecord as VariantToRecord
import Data.String (Pattern(..), contains, trim)
import Effect (Effect)
import PUI (PUI, asField, displayed, forCase, forField, mvu, required, toCases)
import PUI.Web.HTML (atCase, body, staticText, text)
import PUI.Web (Web)
import PUI.Web.MDC3 (bodyMedium, button, card, checkbox, debouncedTextField, elevation5, filledTextField, headlineLarge, radioButton, select, snackbar, titleSmall, tooltip)
import QualifiedDo.Semigroupoid as Semigroupoid

signupFormMDC3 :: Effect Unit
signupFormMDC3 =
  body $
    elevation5 $
      card { caption: "Sign-Up Form" } Semigroupoid.do
        ( RecordToRecord.do
            headlineLarge $ staticText "Create account"
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
        ( bodyMedium $ staticText "Pick a username to check its availability" ) # atCase @"unnamed" usernameStatus # displayed
        ( bodyMedium $ RecordToRecord.do
            staticText "✗ "
            text # forField @"username" identity
            staticText " is already taken" ) # atCase @"taken" usernameStatus # displayed
        ( bodyMedium $ RecordToRecord.do
            staticText "✓ "
            text # forField @"username" identity
            staticText " is available" ) # atCase @"available" usernameStatus # displayed
        ( titleSmall $ RecordToRecord.do
            staticText "⚠ "
            text # forField @"problem" identity ) # atCase @"invalid" validation # displayed
        ( titleSmall $ RecordToRecord.do
            staticText "Ready to sign up as "
            text # forField @"username" identity ) # atCase @"ready" validation # displayed
        button { label: "Sign up", icon: "person_add" } # toCases register
        VariantToRecord.do
          welcomeToast
          rejectionToast

register :: { username :: String, email :: String, plan :: [ free :: {}, pro :: {}, team :: {} ], country :: [ poland :: {}, germany :: {}, france :: {}, spain :: {} ], terms :: Maybe {} } -> [ registered :: String, rejected :: String ]
register applicant = case validate applicant of
  Left problem -> .rejected problem
  Right name -> .registered name

welcomeToast :: PUI Web [ registered :: String ] {}
welcomeToast = snackbar # forCase @"registered" (\name -> "Welcome, " <> name <> "!")

rejectionToast :: PUI Web [ rejected :: String ] {}
rejectionToast = snackbar # forCase @"rejected" (\problem -> "Cannot sign up: " <> problem)

validate :: { username :: String, email :: String, plan :: [ free :: {}, pro :: {}, team :: {} ], country :: [ poland :: {}, germany :: {}, france :: {}, spain :: {} ], terms :: Maybe {} } -> Either String String
validate applicant@{ email, terms } =
  let username = trim applicant.username
  in
    if username == "" then Left "choose a username"
    else if usernameTaken username then Left ("username " <> username <> " is taken")
    else if contains (Pattern "@") email == false then Left "enter a valid email address"
    else if isJust terms == false then Left "accept the terms of service"
    else Right username

validation :: { username :: String, email :: String, plan :: [ free :: {}, pro :: {}, team :: {} ], country :: [ poland :: {}, germany :: {}, france :: {}, spain :: {} ], terms :: Maybe {} } -> [ invalid :: { problem :: String }, ready :: { username :: String } ]
validation = validate >>> either (\problem -> .invalid { problem }) (\username -> .ready { username })

namedUsername :: { username :: String } -> Maybe String
namedUsername { username } = case trim username of
  "" -> Nothing
  name -> Just name

usernameStatus :: { username :: String } -> [ unnamed :: {}, taken :: { username :: String }, available :: { username :: String } ]
usernameStatus { username } = case namedUsername { username } of
  Nothing -> .unnamed {}
  Just name | usernameTaken name -> .taken { username: name }
  Just name -> .available { username: name }

usernameTaken :: String -> Boolean
usernameTaken username = username `elem` takenUsernames

takenUsernames :: Array String
takenUsernames = [ "admin", "root", "guest", "eryk", "bambik" ]

usernameSettleTime :: Number
usernameSettleTime = 300.0

newApplicant :: { username :: String, email :: String, plan :: [ free :: {}, pro :: {}, team :: {} ], country :: [ poland :: {}, germany :: {}, france :: {}, spain :: {} ], terms :: Maybe {} }
newApplicant =
  { username: ""
  , email: ""
  , plan: .free {}
  , country: .poland {}
  , terms: Nothing
  }
