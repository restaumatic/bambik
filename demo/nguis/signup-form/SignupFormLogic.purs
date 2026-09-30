module SignupFormLogic (availableLine, invalidLine, newApplicant, readyLine, signupLine, takenLine, unnamedLine, usernameSettleTime, usernameStatus, validation) where

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

usernameSettleTime :: { ms :: Number }
usernameSettleTime = { ms: 300.0 }

signupLine :: forall r1. { "Username" :: String, "Email" :: String, "Terms" :: [ accepted :: {}, declined :: {} ] | r1 } -> String
signupLine applicant = either rejectionLine welcomeLine (validate applicant)

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

validate :: forall r1. { "Username" :: String, "Email" :: String, "Terms" :: [ accepted :: {}, declined :: {} ] | r1 } -> Either [ unnamed :: {}, taken :: { "Username" :: String }, badEmail :: {}, termsUnaccepted :: {} ] String
validate applicant@{ "Email": email, "Terms": terms } =
  let username = trim applicant."Username"
  in
    if username == "" then Left (.unnamed {})
    else if usernameTaken username then Left (.taken { "Username": username })
    else if not (contains (Pattern "@") email) then Left (.badEmail {})
    else if declined terms then Left (.termsUnaccepted {})
    else Right username

validation :: forall r1. { "Username" :: String, "Email" :: String, "Terms" :: [ accepted :: {}, declined :: {} ] | r1 } -> [ invalid :: { reason :: [ unnamed :: {}, taken :: { "Username" :: String }, badEmail :: {}, termsUnaccepted :: {} ] }, ready :: { "Username" :: String } ]
validation applicant = either (\reason -> .invalid { reason }) (\name -> .ready { "Username": name }) (validate applicant)

invalidLine :: forall r1. { reason :: [ unnamed :: {}, taken :: { "Username" :: String }, badEmail :: {}, termsUnaccepted :: {} ] | r1 } -> String
invalidLine { reason } = "⚠ " <> refusalText reason

readyLine :: forall r1. { "Username" :: String | r1 } -> String
readyLine { "Username": username } = "Ready to sign up as " <> username

usernameStatus :: forall r1. { "Username" :: String | r1 } -> [ unnamed :: {}, taken :: { "Username" :: String }, available :: { "Username" :: String } ]
usernameStatus { "Username": username } = case trim username of
  "" -> .unnamed {}
  name | usernameTaken name -> .taken { "Username": name }
  name -> .available { "Username": name }

takenLine :: forall r1. { "Username" :: String | r1 } -> String
takenLine { "Username": username } = "✗ " <> username <> " is already taken"

availableLine :: forall r1. { "Username" :: String | r1 } -> String
availableLine { "Username": username } = "✓ " <> username <> " is available"

usernameTaken :: String -> Boolean
usernameTaken username = username `elem` takenUsernames

takenUsernames :: Array String
takenUsernames = [ "admin", "root", "guest", "eryk", "bambik" ]

declined :: [ accepted :: {}, declined :: {} ] -> Boolean
declined = match { accepted: const false, declined: const true }

unnamedLine :: forall r1. { | r1 } -> String
unnamedLine _ = "Pick a username to check its availability"
