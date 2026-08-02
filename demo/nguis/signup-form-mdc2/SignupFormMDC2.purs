module SignupFormMDC2 (signupFormMDC2) where

import Prelude (Unit, identity, (#), ($), (<>), (==), (>>>))

import Data.Either (Either(..), either)
import Data.Variant (match)
import Data.Foldable (elem)
import Data.Maybe (Maybe(..), isJust)
import Data.Profunctor.Row.RecordToRecord as RecordToRecord
import Data.Profunctor.Row.VariantToRecord as VariantToRecord
import Data.String (Pattern(..), contains, trim)
import Effect (Effect)
import PUI (PUI, asField, displayed, forCase, forField, mvu, required, toCases)
import PUI.Web.HTML (atCase, body, staticText, text)
import PUI.Web (Web)
import PUI.Web.MDC2 (body2, button, card, checkbox, debouncedTextField, elevation20, filledTextField, headline4, radioButton, select, snackbar, subtitle2, tooltip)
import QualifiedDo.Semigroupoid as Semigroupoid

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

register :: { username :: String, email :: String, plan :: [ free :: {}, pro :: {}, team :: {} ], country :: [ poland :: {}, germany :: {}, france :: {}, spain :: {} ], terms :: Maybe {} } -> [ registered :: String, rejected :: [ unnamed :: {}, taken :: { username :: String }, badEmail :: {}, termsUnaccepted :: {} ] ]
register { username, email, terms } = case validate { username, email, terms } of
  Left problem -> .rejected problem
  Right name -> .registered name

welcomeToast :: PUI Web [ registered :: String ] {}
welcomeToast = snackbar # forCase @"registered" (\name -> "Welcome, " <> name <> "!")

rejectionToast :: PUI Web [ rejected :: [ unnamed :: {}, taken :: { username :: String }, badEmail :: {}, termsUnaccepted :: {} ] ] {}
rejectionToast = snackbar # forCase @"rejected" (\reason -> "Cannot sign up: " <> refusalText reason)

refusalText :: [ unnamed :: {}, taken :: { username :: String }, badEmail :: {}, termsUnaccepted :: {} ] -> String
refusalText = match
  { unnamed: \_ -> "choose a username"
  , taken: \{ username } -> "username " <> username <> " is taken"
  , badEmail: \_ -> "enter a valid email address"
  , termsUnaccepted: \_ -> "accept the terms of service"
  }

validate :: { username :: String, email :: String, terms :: Maybe {} } -> Either [ unnamed :: {}, taken :: { username :: String }, badEmail :: {}, termsUnaccepted :: {} ] String
validate applicant@{ email, terms } =
  let username = trim applicant.username
  in
    if username == "" then Left (.unnamed {})
    else if usernameTaken username then Left (.taken { username })
    else if contains (Pattern "@") email == false then Left (.badEmail {})
    else if isJust terms == false then Left (.termsUnaccepted {})
    else Right username

validation :: { username :: String, email :: String, plan :: [ free :: {}, pro :: {}, team :: {} ], country :: [ poland :: {}, germany :: {}, france :: {}, spain :: {} ], terms :: Maybe {} } -> [ invalid :: { problem :: String }, ready :: { username :: String } ]
validation { username, email, terms } = either (\reason -> .invalid { problem: refusalText reason }) (\name -> .ready { username: name }) (validate { username, email, terms })

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
