module SignupForm (signupForm) where

import Prelude ((#), ($), (<>), (==), (>>>), Unit, not)

import Data.Either (Either(..), either)
import Data.Foldable (elem)
import Data.Maybe (Maybe(..), isJust)
import Data.Profunctor (lcmap, rmap)
import Data.Profunctor.Row.RecordToRecord as RecordToRecord
import Data.Profunctor.Row.VariantToRecord as VariantToRecord
import Data.String (Pattern(..), contains, trim)
import Data.Variant (match)
import Effect (Effect)
import PUI (PUI, asCase, asField, displayed, forCase, forField, forValue, mvu, required)
import PUI.HTML (body, provided, staticText, text)
import PUI.Web (Web)
import PUI.MDC (body2, button, card, checkbox, debouncedTextField, elevation20, filledTextField, headline4, radioButton, select, snackbar, subtitle2, tooltip)
import QualifiedDo.Semigroupoid as Semigroupoid

signupForm :: Effect Unit
signupForm =
  body $
    elevation20 $
      card { caption: "Sign-Up Form" } Semigroupoid.do
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
        ( body2 $ staticText "Pick a username to check its availability" ) # provided # lcmap whenUnnamed # displayed
        ( body2 $ RecordToRecord.do
            staticText "✗ "
            text # forValue # forField @"username"
            staticText " is already taken" ) # provided # lcmap whenTaken # displayed
        ( body2 $ RecordToRecord.do
            staticText "✓ "
            text # forValue # forField @"username"
            staticText " is available" ) # provided # lcmap whenAvailable # displayed
        ( subtitle2 $ RecordToRecord.do
            staticText "⚠ "
            text # forValue # forField @"problem" ) # provided # lcmap whenInvalid # displayed
        ( subtitle2 $ RecordToRecord.do
            staticText "Ready to sign up as "
            text # forValue # forField @"username" ) # provided # lcmap whenReady # displayed
        button { label: "Sign up", icon: "person_add" } # asCase @"signUp" # rmap (match { signUp: register })
        VariantToRecord.do
          welcomeToast
          rejectionToast

register :: { username :: String, email :: String, plan :: [ free :: {}, pro :: {}, team :: {} ], country :: [ poland :: {}, germany :: {}, france :: {}, spain :: {} ], terms :: Maybe {} } -> [ registered :: String, rejected :: String ]
register applicant = case validate applicant of
  Left problem -> .rejected problem
  Right name -> .registered name

welcomeToast :: PUI Web [ registered :: String ] {}
welcomeToast = snackbar # forCase @"registered" # lcmap (match { registered: \name -> .registered ("Welcome, " <> name <> "!") })

rejectionToast :: PUI Web [ rejected :: String ] {}
rejectionToast = snackbar # forCase @"rejected" # lcmap (match { rejected: \problem -> .rejected ("Cannot sign up: " <> problem) })

validate :: { username :: String, email :: String, plan :: [ free :: {}, pro :: {}, team :: {} ], country :: [ poland :: {}, germany :: {}, france :: {}, spain :: {} ], terms :: Maybe {} } -> Either String String
validate applicant@{ email, terms } =
  let username = trim applicant.username
  in
    if username == "" then Left "choose a username"
    else if usernameTaken username then Left ("username " <> username <> " is taken")
    else if contains (Pattern "@") email == false then Left "enter a valid email address"
    else if isJust terms == false then Left "accept the terms of service"
    else Right username

whenInvalid :: { username :: String, email :: String, plan :: [ free :: {}, pro :: {}, team :: {} ], country :: [ poland :: {}, germany :: {}, france :: {}, spain :: {} ], terms :: Maybe {} } -> Maybe { problem :: String }
whenInvalid = validate >>> either (\problem -> Just { problem }) (\_ -> Nothing)

whenReady :: { username :: String, email :: String, plan :: [ free :: {}, pro :: {}, team :: {} ], country :: [ poland :: {}, germany :: {}, france :: {}, spain :: {} ], terms :: Maybe {} } -> Maybe { username :: String }
whenReady = validate >>> either (\_ -> Nothing) (\username -> Just { username })

namedUsername :: { username :: String } -> Maybe String
namedUsername { username } = case trim username of
  "" -> Nothing
  name -> Just name

whenUnnamed :: { username :: String } -> Maybe {}
whenUnnamed { username } = case namedUsername { username } of
  Nothing -> Just {}
  Just _ -> Nothing

whenTaken :: { username :: String } -> Maybe { username :: String }
whenTaken { username } = case namedUsername { username } of
  Just name | usernameTaken name -> Just { username: name }
  _ -> Nothing

whenAvailable :: { username :: String } -> Maybe { username :: String }
whenAvailable { username } = case namedUsername { username } of
  Just name | not (usernameTaken name) -> Just { username: name }
  _ -> Nothing

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
