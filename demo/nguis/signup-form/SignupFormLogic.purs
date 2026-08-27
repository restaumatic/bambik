module SignupFormLogic (newApplicant, register, rejectionLine, usernameSettleTime, usernameStatus, validation, welcomeLine) where

import Prelude ((<>), (==))

import Data.Either (Either(..), either)
import Data.Foldable (elem)
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..), contains, trim)
import Data.Variant (match)

newApplicant :: { "Username" :: String, "Email" :: String, "Plan" :: [ "Free" :: {}, "Pro" :: {}, "Team" :: {} ], "Country" :: [ "Poland" :: {}, "Germany" :: {}, "France" :: {}, "Spain" :: {} ], "Terms" :: [ accepted :: {}, declined :: {} ] }
newApplicant =
  { "Username": ""
  , "Email": ""
  , "Plan": ."Free" {}
  , "Country": ."Poland" {}
  , "Terms": .declined {}
  }

usernameSettleTime :: Number
usernameSettleTime = 300.0

register :: { "Username" :: String, "Email" :: String, "Plan" :: [ "Free" :: {}, "Pro" :: {}, "Team" :: {} ], "Country" :: [ "Poland" :: {}, "Germany" :: {}, "France" :: {}, "Spain" :: {} ], "Terms" :: [ accepted :: {}, declined :: {} ] } -> [ registered :: String, rejected :: [ unnamed :: {}, taken :: { "Username" :: String }, badEmail :: {}, termsUnaccepted :: {} ] ]
register { "Username": username, "Email": email, "Terms": terms } = case validate { "Username": username, "Email": email, "Terms": terms } of
  Left problem -> .rejected problem
  Right name -> .registered name

welcomeLine :: String -> String
welcomeLine name = "Welcome, " <> name <> "!"

rejectionLine :: [ unnamed :: {}, taken :: { "Username" :: String }, badEmail :: {}, termsUnaccepted :: {} ] -> String
rejectionLine reason = "Cannot sign up: " <> refusalText reason

refusalText :: [ unnamed :: {}, taken :: { "Username" :: String }, badEmail :: {}, termsUnaccepted :: {} ] -> String
refusalText = match
  { unnamed: \_ -> "choose a username"
  , taken: \{ "Username": username } -> "username " <> username <> " is taken"
  , badEmail: \_ -> "enter a valid email address"
  , termsUnaccepted: \_ -> "accept the terms of service"
  }

validate :: { "Username" :: String, "Email" :: String, "Terms" :: [ accepted :: {}, declined :: {} ] } -> Either [ unnamed :: {}, taken :: { "Username" :: String }, badEmail :: {}, termsUnaccepted :: {} ] String
validate applicant@{ "Email": email, "Terms": terms } =
  let username = trim applicant."Username"
  in
    if username == "" then Left (.unnamed {})
    else if usernameTaken username then Left (.taken { "Username": username })
    else if contains (Pattern "@") email == false then Left (.badEmail {})
    else if declined terms then Left (.termsUnaccepted {})
    else Right username

validation :: { "Username" :: String, "Email" :: String, "Plan" :: [ "Free" :: {}, "Pro" :: {}, "Team" :: {} ], "Country" :: [ "Poland" :: {}, "Germany" :: {}, "France" :: {}, "Spain" :: {} ], "Terms" :: [ accepted :: {}, declined :: {} ] } -> [ invalid :: { problem :: String }, ready :: { "Username" :: String } ]
validation { "Username": username, "Email": email, "Terms": terms } = either (\reason -> .invalid { problem: refusalText reason }) (\name -> .ready { "Username": name }) (validate { "Username": username, "Email": email, "Terms": terms })

namedUsername :: { "Username" :: String } -> Maybe String
namedUsername { "Username": username } = case trim username of
  "" -> Nothing
  name -> Just name

usernameStatus :: { "Username" :: String } -> [ unnamed :: {}, taken :: { "Username" :: String }, available :: { "Username" :: String } ]
usernameStatus { "Username": username } = case namedUsername { "Username": username } of
  Nothing -> .unnamed {}
  Just name | usernameTaken name -> .taken { "Username": name }
  Just name -> .available { "Username": name }

usernameTaken :: String -> Boolean
usernameTaken username = username `elem` takenUsernames

takenUsernames :: Array String
takenUsernames = [ "admin", "root", "guest", "eryk", "bambik" ]

declined :: [ accepted :: {}, declined :: {} ] -> Boolean
declined = match { accepted: \_ -> false, declined: \_ -> true }
