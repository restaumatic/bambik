module PasswordGeneratorMDC3 (passwordGeneratorMDC3) where

import Prelude ((#), ($), (*), (-), (/), (<), (<>), (>>>), Unit, bind, otherwise, pure)

import Data.Array (index, length, null, replicate)
import Data.Int (round, toNumber)
import Data.Maybe (fromMaybe)
import Data.Number (log)
import Data.String.CodeUnits (fromCharArray, toCharArray)
import Data.Traversable (sequence)
import Data.Variant (match)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Random (randomInt)
import PUI (action, asCase, asField, completed, forField, forValue, mvu, onCase, projection, tapped, updates)
import PUI.HTML (attr, body, div, staticText, text)
import PUI.MDC3 (bodyMedium, button, card, elevation5, indeterminateLinearProgress, slider, toggleSwitch)
import Data.Profunctor.Row.RecordToRecord as RecordToRecord
import QualifiedDo.Semigroupoid as Semigroupoid

passwordGeneratorMDC3 :: Effect Unit
passwordGeneratorMDC3 =
  body $
    elevation5 $
      card { caption: "Password Generator" } $ ( Semigroupoid.do
          ( RecordToRecord.do
              slider { label: "Length", min: minLength, max: maxLength, step: lengthStep } # asField @"length"
              toggleSwitch { label: "Uppercase letters" } # asField @"uppercase"
              toggleSwitch { label: "Lowercase letters" } # asField @"lowercase"
              toggleSwitch { label: "Digits" } # asField @"digits"
              toggleSwitch { label: "Symbols" } # asField @"symbols") # completed
          bodyMedium ( RecordToRecord.do
              staticText "Strength: "
              text # projection strengthText ) # tapped
          div >>> attr "style" "font-family: monospace; font-size: 1.2rem; word-break: break-all; min-height: 1.6rem; margin: 8px 0;" >>> attr "id" "password" $
            text # forValue # forField @"password" # tapped
          ( Semigroupoid.do
              button { label: "Generate" } # asCase @"generate"
              indeterminateLinearProgress # action samplePassword # onCase @"generate") # updates (match { generated: rememberPassword })
      ) # mvu strongMixRecipe

samplePassword :: { length :: Number, uppercase :: Boolean, lowercase :: Boolean, digits :: Boolean, symbols :: Boolean } -> Aff [ generated :: String ]
samplePassword { length, uppercase, lowercase, digits, symbols } = liftEffect do
  let alphabet = effectiveAlphabet { uppercase, lowercase, digits, symbols }
  chars <- sequence (replicate (round length) (randomCharacter alphabet))
  pure (.generated (fromCharArray chars))

randomCharacter :: Array Char -> Effect Char
randomCharacter alphabet = do
  i <- randomInt 0 (length alphabet - 1)
  pure (fromMaybe 'a' (index alphabet i))

rememberPassword :: String -> { password :: String } -> { password :: String }
rememberPassword password recipe = recipe { password = password }

effectiveAlphabet :: { uppercase :: Boolean, lowercase :: Boolean, digits :: Boolean, symbols :: Boolean } -> Array Char
effectiveAlphabet { uppercase, lowercase, digits, symbols } =
  let chosen = (if uppercase then uppercaseLetters else [])
            <> (if lowercase then lowercaseLetters else [])
            <> (if digits then digitCharacters else [])
            <> (if symbols then symbolCharacters else [])
  in if null chosen then lowercaseLetters else chosen

strengthText :: { length :: Number, uppercase :: Boolean, lowercase :: Boolean, digits :: Boolean, symbols :: Boolean } -> String
strengthText { length, uppercase, lowercase, digits, symbols } = strengthGrade (entropyBits { length, uppercase, lowercase, digits, symbols })

entropyBits :: { length :: Number, uppercase :: Boolean, lowercase :: Boolean, digits :: Boolean, symbols :: Boolean } -> Number
entropyBits { length: len, uppercase, lowercase, digits, symbols } = len * log (toNumber (length (effectiveAlphabet { uppercase, lowercase, digits, symbols }))) / log 2.0

strengthGrade :: Number -> String
strengthGrade bits
  | bits < 45.0 = "weak"
  | bits < 70.0 = "fair"
  | bits < 100.0 = "strong"
  | otherwise = "very strong"

uppercaseLetters :: Array Char
uppercaseLetters = toCharArray "ABCDEFGHIJKLMNOPQRSTUVWXYZ"

lowercaseLetters :: Array Char
lowercaseLetters = toCharArray "abcdefghijklmnopqrstuvwxyz"

digitCharacters :: Array Char
digitCharacters = toCharArray "0123456789"

symbolCharacters :: Array Char
symbolCharacters = toCharArray "!@#$%^&*()-_=+[]{};:,.<>?/"

strongMixRecipe :: { length :: Number, uppercase :: Boolean, lowercase :: Boolean, digits :: Boolean, symbols :: Boolean, password :: String }
strongMixRecipe =
  { length: 16.0
  , uppercase: true
  , lowercase: true
  , digits: true
  , symbols: false
  , password: ""
  }

minLength :: Number
minLength = 8.0

maxLength :: Number
maxLength = 64.0

lengthStep :: Number
lengthStep = 1.0
