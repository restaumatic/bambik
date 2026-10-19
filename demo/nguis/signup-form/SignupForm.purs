module SignupForm (Applicant, Consent, Country, Plan, availableLine, blankEmail, blankUsername, freePlan, homeCountry, invalidLine, newApplicant, readyLine, signUp, termsDeclined, usernameSettleTime, usernameStatus, validation) where

import Prelude (const, discard, not, pure, (<>), (==))

import Data.Either (Either(..), either)
import Data.Foldable (elem)
import Data.String (Pattern(..), contains, trim)
import Data.Variant (match)
import Effect.Aff (Aff, Milliseconds(..), delay)

type Applicant =
  { "Username" :: String
  , "Email" :: String
  , "Plan" :: Plan
  , "Country" :: Country
  , "Accept the terms of service" :: Consent
  }

type Plan = [ "Free" :: {}, "Pro" :: {}, "Team" :: {} ]

type Country = [ "France" :: {}, "Germany" :: {}, "Poland" :: {}, "Spain" :: {} ]

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

blankUsername :: {} -> String
blankUsername _ = ""

freePlan :: { "Username" :: String } -> Plan
freePlan _ = ."Free" {}

homeCountry :: { "Username" :: String, "Plan" :: Plan } -> Country
homeCountry _ = ."Poland" {}

blankEmail :: { "Username" :: String, "Plan" :: Plan, "Country" :: Country } -> String
blankEmail _ = ""

termsDeclined
  :: { "Username" :: String, "Plan" :: Plan, "Country" :: Country, "Email" :: String }
  -> Consent
termsDeclined _ = .declined {}

usernameSettleTime :: { ms :: Number }
usernameSettleTime = { ms: 300.0 }

refusalText :: Refusal -> String
refusalText = match
  { unnamed: const "Choose a username"
  , taken: \{ "Username": username } -> "Username " <> username <> " is already taken"
  , badEmail: const "Enter a valid email address"
  , termsUnaccepted: const "Accept the terms of service"
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

usernameStatus :: Applicant -> [ available :: {}, unavailable :: {} ]
usernameStatus { "Username": username } = case trim username of
  "" -> .unavailable {}
  name | usernameTaken name -> .unavailable {}
  _ -> .available {}

availableLine :: Applicant -> String
availableLine { "Username": username } = "✓ " <> trim username <> " is available"

usernameTaken :: String -> Boolean
usernameTaken username = username `elem` takenUsernames

takenUsernames :: Array String
takenUsernames = [ "admin", "root", "guest", "eryk", "bambik" ]

declined :: Consent -> Boolean
declined = match { accepted: const false, declined: const true }

