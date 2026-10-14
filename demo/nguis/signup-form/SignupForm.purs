module SignupForm (availableLine, invalidLine, newApplicant, readyLine, signUp, takenLine, usernameSettleTime, usernameStatus, validation) where

import Prelude (const, discard, not, pure, (<>), (==))

import Data.Either (Either(..), either)
import Data.Foldable (elem)
import Data.String (Pattern(..), contains, trim)
import Data.Variant (match)
import Effect.Aff (Aff, Milliseconds(..), delay)

newApplicant
  :: { "Country" :: [ "France" :: {}, "Germany" :: {}, "Poland" :: {}, "Spain" :: {} ]
     , "Email" :: String
     , "I accept the terms of service" :: [ accepted :: {}, declined :: {} ]
     , "Plan" :: [ "Free" :: {}, "Pro" :: {}, "Team" :: {} ]
     , "Username" :: String
     }
newApplicant =
  { "Username": ""
  , "Email": ""
  , "Plan": ."Free" {}
  , "Country": ."Poland" {}
  , "I accept the terms of service": .declined {}
  }

usernameSettleTime :: { ms :: Number }
usernameSettleTime = { ms: 300.0 }

refusalText :: [ unnamed :: {}, taken :: { "Username" :: String }, badEmail :: {}, termsUnaccepted :: {} ] -> String
refusalText = match
  { unnamed: const "choose a username"
  , taken: \{ "Username": username } -> "username " <> username <> " is taken"
  , badEmail: const "enter a valid email address"
  , termsUnaccepted: const "accept the terms of service"
  }

validate :: forall r1. { "Username" :: String, "Email" :: String, "I accept the terms of service" :: [ accepted :: {}, declined :: {} ] | r1 } -> Either [ unnamed :: {}, taken :: { "Username" :: String }, badEmail :: {}, termsUnaccepted :: {} ] String
validate applicant@{ "Email": email, "I accept the terms of service": terms } =
  let username = trim applicant."Username"
  in
    if username == "" then Left (.unnamed {})
    else if usernameTaken username then Left (.taken { "Username": username })
    else if not (contains (Pattern "@") email) then Left (.badEmail {})
    else if declined terms then Left (.termsUnaccepted {})
    else Right username

signUp
  :: { "Country" :: [ "France" :: {}, "Germany" :: {}, "Poland" :: {}, "Spain" :: {} ]
     , "Email" :: String
     , "I accept the terms of service" :: [ accepted :: {}, declined :: {} ]
     , "Plan" :: [ "Free" :: {}, "Pro" :: {}, "Team" :: {} ]
     , "Username" :: String
     }
  -> Aff [ "Account created" :: { "Country" :: [ "France" :: {}, "Germany" :: {}, "Poland" :: {}, "Spain" :: {} ]
                                , "Email" :: String
                                , "I accept the terms of service" :: [ accepted :: {}, declined :: {} ]
                                , "Plan" :: [ "Free" :: {}, "Pro" :: {}, "Team" :: {} ]
                                , "Username" :: String
                                }
         ]
signUp applicant = do
  delay (Milliseconds signupDelay.ms)
  pure (."Account created" applicant)

signupDelay :: { ms :: Number }
signupDelay = { ms: 800.0 }

validation
  :: { "Country" :: [ "France" :: {}, "Germany" :: {}, "Poland" :: {}, "Spain" :: {} ]
     , "Email" :: String
     , "I accept the terms of service" :: [ accepted :: {}, declined :: {} ]
     , "Plan" :: [ "Free" :: {}, "Pro" :: {}, "Team" :: {} ]
     , "Username" :: String
     }
  -> [ invalid :: {}, ready :: {} ]
validation applicant = either (const (.invalid {})) (const (.ready {})) (validate applicant)

invalidLine
  :: { "Country" :: [ "France" :: {}, "Germany" :: {}, "Poland" :: {}, "Spain" :: {} ]
     , "Email" :: String
     , "I accept the terms of service" :: [ accepted :: {}, declined :: {} ]
     , "Plan" :: [ "Free" :: {}, "Pro" :: {}, "Team" :: {} ]
     , "Username" :: String
     }
  -> String
invalidLine applicant = either (\reason -> "⚠ " <> refusalText reason) (const "") (validate applicant)

readyLine
  :: { "Country" :: [ "France" :: {}, "Germany" :: {}, "Poland" :: {}, "Spain" :: {} ]
     , "Email" :: String
     , "I accept the terms of service" :: [ accepted :: {}, declined :: {} ]
     , "Plan" :: [ "Free" :: {}, "Pro" :: {}, "Team" :: {} ]
     , "Username" :: String
     }
  -> String
readyLine applicant = either (const "") (\name -> "Ready to sign up as " <> name) (validate applicant)

usernameStatus
  :: { "Country" :: [ "France" :: {}, "Germany" :: {}, "Poland" :: {}, "Spain" :: {} ]
     , "Email" :: String
     , "I accept the terms of service" :: [ accepted :: {}, declined :: {} ]
     , "Plan" :: [ "Free" :: {}, "Pro" :: {}, "Team" :: {} ]
     , "Username" :: String
     }
  -> [ available :: {}, taken :: {}, unnamed :: {} ]
usernameStatus { "Username": username } = case trim username of
  "" -> .unnamed {}
  name | usernameTaken name -> .taken {}
  _ -> .available {}

takenLine
  :: { "Country" :: [ "France" :: {}, "Germany" :: {}, "Poland" :: {}, "Spain" :: {} ]
     , "Email" :: String
     , "I accept the terms of service" :: [ accepted :: {}, declined :: {} ]
     , "Plan" :: [ "Free" :: {}, "Pro" :: {}, "Team" :: {} ]
     , "Username" :: String
     }
  -> String
takenLine { "Username": username } = "✗ " <> trim username <> " is already taken"

availableLine
  :: { "Country" :: [ "France" :: {}, "Germany" :: {}, "Poland" :: {}, "Spain" :: {} ]
     , "Email" :: String
     , "I accept the terms of service" :: [ accepted :: {}, declined :: {} ]
     , "Plan" :: [ "Free" :: {}, "Pro" :: {}, "Team" :: {} ]
     , "Username" :: String
     }
  -> String
availableLine { "Username": username } = "✓ " <> trim username <> " is available"

usernameTaken :: String -> Boolean
usernameTaken username = username `elem` takenUsernames

takenUsernames :: Array String
takenUsernames = [ "admin", "root", "guest", "eryk", "bambik" ]

declined :: [ accepted :: {}, declined :: {} ] -> Boolean
declined = match { accepted: const false, declined: const true }

