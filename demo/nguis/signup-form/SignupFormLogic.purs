module SignupFormLogic (newApplicant, register, rejectionLine, usernameSettleTime, usernameStatus, validation, welcomeLine) where

import Prelude ((<>), (==))

import Data.Either (Either(..), either)
import Data.Foldable (elem)
import Data.Maybe (Maybe(..), isJust)
import Data.String (Pattern(..), contains, trim)
import Data.Variant (match)

newApplicant :: { username :: String, email :: String, plan :: [ free :: {}, pro :: {}, team :: {} ], country :: [ poland :: {}, germany :: {}, france :: {}, spain :: {} ], terms :: Maybe {} }
newApplicant =
  { username: ""
  , email: ""
  , plan: .free {}
  , country: .poland {}
  , terms: Nothing
  }

usernameSettleTime :: Number
usernameSettleTime = 300.0

register :: { username :: String, email :: String, plan :: [ free :: {}, pro :: {}, team :: {} ], country :: [ poland :: {}, germany :: {}, france :: {}, spain :: {} ], terms :: Maybe {} } -> [ registered :: String, rejected :: [ unnamed :: {}, taken :: { username :: String }, badEmail :: {}, termsUnaccepted :: {} ] ]
register { username, email, terms } = case validate { username, email, terms } of
  Left problem -> .rejected problem
  Right name -> .registered name

welcomeLine :: String -> String
welcomeLine name = "Welcome, " <> name <> "!"

rejectionLine :: [ unnamed :: {}, taken :: { username :: String }, badEmail :: {}, termsUnaccepted :: {} ] -> String
rejectionLine reason = "Cannot sign up: " <> refusalText reason

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
