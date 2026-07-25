module SignupForm (signupForm) where

import Prelude ((#), ($), (<>), (==), (>>>), Unit, not)

import Data.Either (Either(..), either)
import Data.Foldable (elem)
import Data.Maybe (Maybe(..), isJust)
import Data.Profunctor (lcmap, rmap)
import Data.Profunctor.Row.RecordToRecord as RecordToRecord
import Data.Profunctor.Row.VariantToRecord as VariantToRecord
import Data.String (Pattern(..), contains, trim)
import Data.Time.Duration (Milliseconds(..))
import Data.Variant (match)
import Effect (Effect)
import PUI (asCase, asField, displayed, forCase, forField, forValue, mvu, required)
import PUI.HTML (body, provided, staticText, text)
import PUI.MDC (body2, button, card, checkbox, debouncedTextField, elevation20, filledTextField, headline4, radioButton, select, snackbar, subtitle2, tooltip)
import QualifiedDo.Semigroupoid as Semigroupoid

signupForm :: Effect Unit
signupForm =
  body $
    elevation20 $
      card { caption: "Sign-Up Form" } Semigroupoid.do
        ( RecordToRecord.do
            headline4 $ staticText "Create account"
            debouncedTextField { floatingLabel: "Username", millis: usernameSettleTime } # asField @"username"
            radioButton
              [ { value: "free", label: "Free plan" }
              , { value: "pro", label: "Pro plan" }
              , { value: "team", label: "Team plan" }
              ] # required # asField @"plan"
            select { floatingLabel: "Country" }
              [ { value: "Poland", label: "Poland" }
              , { value: "Germany", label: "Germany" }
              , { value: "France", label: "France" }
              , { value: "Spain", label: "Spain" }
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
          snackbar # forCase @"registered"
          snackbar # forCase @"rejected"

register :: { username :: String, email :: String, plan :: String, country :: String, terms :: Maybe Unit } -> [ registered :: String, rejected :: String ]
register applicant = case validate applicant of
  Left problem -> .rejected ("Cannot sign up: " <> problem)
  Right username -> .registered ("Welcome, " <> username <> "!")

validate :: { username :: String, email :: String, plan :: String, country :: String, terms :: Maybe Unit } -> Either String String
validate applicant =
  let username = trim applicant.username
  in
    if username == "" then Left "choose a username"
    else if usernameTaken username then Left ("username " <> username <> " is taken")
    else if contains (Pattern "@") applicant.email == false then Left "enter a valid email address"
    else if isJust applicant.terms == false then Left "accept the terms of service"
    else Right username

whenInvalid :: { username :: String, email :: String, plan :: String, country :: String, terms :: Maybe Unit } -> Maybe { problem :: String }
whenInvalid = validate >>> either (\problem -> Just { problem }) (\_ -> Nothing)

whenReady :: { username :: String, email :: String, plan :: String, country :: String, terms :: Maybe Unit } -> Maybe { username :: String }
whenReady = validate >>> either (\_ -> Nothing) (\username -> Just { username })

namedUsername :: { username :: String } -> Maybe String
namedUsername applicant = case trim applicant.username of
  "" -> Nothing
  username -> Just username

whenUnnamed :: { username :: String } -> Maybe {}
whenUnnamed applicant = case namedUsername applicant of
  Nothing -> Just {}
  Just _ -> Nothing

whenTaken :: { username :: String } -> Maybe { username :: String }
whenTaken applicant = case namedUsername applicant of
  Just username | usernameTaken username -> Just { username }
  _ -> Nothing

whenAvailable :: { username :: String } -> Maybe { username :: String }
whenAvailable applicant = case namedUsername applicant of
  Just username | not (usernameTaken username) -> Just { username }
  _ -> Nothing

usernameTaken :: String -> Boolean
usernameTaken username = username `elem` takenUsernames

takenUsernames :: Array String
takenUsernames = [ "admin", "root", "guest", "eryk", "bambik" ]

usernameSettleTime :: Milliseconds
usernameSettleTime = Milliseconds 300.0

newApplicant :: { username :: String, email :: String, plan :: String, country :: String, terms :: Maybe Unit }
newApplicant =
  { username: ""
  , email: ""
  , plan: "free"
  , country: "Poland"
  , terms: Nothing
  }
