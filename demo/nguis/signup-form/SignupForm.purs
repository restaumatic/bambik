module SignupForm (Applicant, Consent, availableLine, invalidLine, newApplicant, readyLine, signUp, takenLine, usernameSettleTime, usernameStatus, validation) where

import Prelude (const, discard, not, pure, (<>), (==))

import Data.Either (Either(..), either)
import Data.Foldable (elem)
import Data.String (Pattern(..), contains, trim)
import Data.Variant (match)
import Effect.Aff (Aff, Milliseconds(..), delay)

type Applicant =
  { "Username" :: String
  , "Email" :: String
  , "Plan" :: [ "Free" :: {}, "Pro" :: {}, "Team" :: {} ]
  , "Country" :: [ "France" :: {}, "Germany" :: {}, "Poland" :: {}, "Spain" :: {} ]
  , "Accept the terms of service" :: Consent
  }

type Consent = [ accepted :: {}, declined :: {} ]

type Refusal =
  [ unnamed :: {}
  , taken :: { "Username" :: String }
  , badEmail :: {}
  , termsUnaccepted :: {}
  ]

newApplicant :: Applicant
newApplicant =
  { "Username": ""
  , "Email": ""
  , "Plan": ."Free" {}
  , "Country": ."Poland" {}
  , "Accept the terms of service": .declined {}
  }

usernameSettleTime :: { ms :: Number }
usernameSettleTime = { ms: 300.0 }

refusalText :: Refusal -> String
refusalText = match
  { unnamed: const "choose a username"
  , taken: \{ "Username": username } -> "username " <> username <> " is taken"
  , badEmail: const "enter a valid email address"
  , termsUnaccepted: const "accept the terms of service"
  }

validate
  :: forall r1.
     { "Username" :: String, "Email" :: String, "Accept the terms of service" :: Consent | r1 }
  -> Either Refusal String
validate applicant@{ "Email": email, "Accept the terms of service": terms } =
  let username = trim applicant."Username"
  in
    if username == "" then Left (.unnamed {})
    else if usernameTaken username then Left (.taken { "Username": username })
    else if not (contains (Pattern "@") email) then Left (.badEmail {})
    else if declined terms then Left (.termsUnaccepted {})
    else Right username

signUp :: Applicant -> Aff [ "Account created" :: Applicant ]
signUp applicant = do
  delay (Milliseconds signupDelay.ms)
  pure (."Account created" applicant)

signupDelay :: { ms :: Number }
signupDelay = { ms: 800.0 }

validation :: Applicant -> [ invalid :: {}, ready :: {} ]
validation applicant = either (const (.invalid {})) (const (.ready {})) (validate applicant)

invalidLine :: Applicant -> String
invalidLine applicant = either (\reason -> "⚠ " <> refusalText reason) (const "") (validate applicant)

readyLine :: Applicant -> String
readyLine applicant = either (const "") (\name -> "Ready to sign up as " <> name) (validate applicant)

usernameStatus :: Applicant -> [ available :: {}, taken :: {}, unnamed :: {} ]
usernameStatus { "Username": username } = case trim username of
  "" -> .unnamed {}
  name | usernameTaken name -> .taken {}
  _ -> .available {}

takenLine :: Applicant -> String
takenLine { "Username": username } = "✗ " <> trim username <> " is already taken"

availableLine :: Applicant -> String
availableLine { "Username": username } = "✓ " <> trim username <> " is available"

usernameTaken :: String -> Boolean
usernameTaken username = username `elem` takenUsernames

takenUsernames :: Array String
takenUsernames = [ "admin", "root", "guest", "eryk", "bambik" ]

declined :: Consent -> Boolean
declined = match { accepted: const false, declined: const true }

