module SignupFormLogic (availableLine, invalidLine, newApplicant, readyLine, register, rejectionLine, takenLine, usernameSettleTime, usernameStatus, validation, welcomeLine) where

import Prelude (const, not, (<>), (==))

import Data.Either (Either(..), either)
import Data.Foldable (elem)
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

register :: { "Username" :: String, "Email" :: String, "Terms" :: [ accepted :: {}, declined :: {} ] } -> [ registered :: String, rejected :: [ unnamed :: {}, taken :: { "Username" :: String }, badEmail :: {}, termsUnaccepted :: {} ] ]
register applicant = case validate applicant of
  Left problem -> .rejected problem
  Right name -> .registered name

welcomeLine :: String -> String
welcomeLine name = "Welcome, " <> name <> "!"

rejectionLine :: [ unnamed :: {}, taken :: { "Username" :: String }, badEmail :: {}, termsUnaccepted :: {} ] -> String
rejectionLine reason = "Cannot sign up: " <> refusalText reason

refusalText :: [ unnamed :: {}, taken :: { "Username" :: String }, badEmail :: {}, termsUnaccepted :: {} ] -> String
refusalText = match
  { unnamed: const "choose a username"
  , taken: \{ "Username": username } -> "username " <> username <> " is taken"
  , badEmail: const "enter a valid email address"
  , termsUnaccepted: const "accept the terms of service"
  }

validate :: { "Username" :: String, "Email" :: String, "Terms" :: [ accepted :: {}, declined :: {} ] } -> Either [ unnamed :: {}, taken :: { "Username" :: String }, badEmail :: {}, termsUnaccepted :: {} ] String
validate applicant@{ "Email": email, "Terms": terms } =
  let username = trim applicant."Username"
  in
    if username == "" then Left (.unnamed {})
    else if usernameTaken username then Left (.taken { "Username": username })
    else if not (contains (Pattern "@") email) then Left (.badEmail {})
    else if declined terms then Left (.termsUnaccepted {})
    else Right username

validation :: { "Username" :: String, "Email" :: String, "Terms" :: [ accepted :: {}, declined :: {} ] } -> [ invalid :: { reason :: [ unnamed :: {}, taken :: { "Username" :: String }, badEmail :: {}, termsUnaccepted :: {} ] }, ready :: { "Username" :: String } ]
validation applicant = either (\reason -> .invalid { reason }) (\name -> .ready { "Username": name }) (validate applicant)

invalidLine :: { reason :: [ unnamed :: {}, taken :: { "Username" :: String }, badEmail :: {}, termsUnaccepted :: {} ] } -> String
invalidLine { reason } = "⚠ " <> refusalText reason

readyLine :: { "Username" :: String } -> String
readyLine { "Username": username } = "Ready to sign up as " <> username

usernameStatus :: { "Username" :: String } -> [ unnamed :: {}, taken :: { "Username" :: String }, available :: { "Username" :: String } ]
usernameStatus { "Username": username } = case trim username of
  "" -> .unnamed {}
  name | usernameTaken name -> .taken { "Username": name }
  name -> .available { "Username": name }

takenLine :: { "Username" :: String } -> String
takenLine { "Username": username } = "✗ " <> username <> " is already taken"

availableLine :: { "Username" :: String } -> String
availableLine { "Username": username } = "✓ " <> username <> " is available"

usernameTaken :: String -> Boolean
usernameTaken username = username `elem` takenUsernames

takenUsernames :: Array String
takenUsernames = [ "admin", "root", "guest", "eryk", "bambik" ]

declined :: [ accepted :: {}, declined :: {} ] -> Boolean
declined = match { accepted: const false, declined: const true }
