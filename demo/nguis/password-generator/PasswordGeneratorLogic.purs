module PasswordGeneratorLogic (rememberPassword, samplePassword, strengthText, strongMixRecipe) where

import Prelude ((*), (-), (/), (<), (<>), bind, otherwise, pure)

import Data.Array (index, length, null, replicate)
import Data.Int (round, toNumber)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Number (log)
import Data.String.CodeUnits (fromCharArray, toCharArray)
import Data.Traversable (sequence)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Random (randomInt)

strongMixRecipe :: { length :: { current :: Number, min :: Number, max :: Number, step :: Maybe Number }, uppercase :: Boolean, lowercase :: Boolean, digits :: Boolean, symbols :: Boolean, password :: String }
strongMixRecipe =
  { length: passwordLengths 16.0
  , uppercase: true
  , lowercase: true
  , digits: true
  , symbols: false
  , password: ""
  }

passwordLengths :: Number -> { current :: Number, min :: Number, max :: Number, step :: Maybe Number }
passwordLengths n = { current: n, min: 8.0, max: 64.0, step: Just 1.0 }

samplePassword :: { length :: { current :: Number, min :: Number, max :: Number, step :: Maybe Number }, uppercase :: Boolean, lowercase :: Boolean, digits :: Boolean, symbols :: Boolean } -> Aff [ generated :: String ]
samplePassword { length, uppercase, lowercase, digits, symbols } = liftEffect do
  let alphabet = effectiveAlphabet { uppercase, lowercase, digits, symbols }
  chars <- sequence (replicate (round length.current) (randomCharacter alphabet))
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

strengthText :: { length :: { current :: Number, min :: Number, max :: Number, step :: Maybe Number }, uppercase :: Boolean, lowercase :: Boolean, digits :: Boolean, symbols :: Boolean } -> String
strengthText { length, uppercase, lowercase, digits, symbols } = strengthGrade (entropyBits { length, uppercase, lowercase, digits, symbols })

entropyBits :: { length :: { current :: Number, min :: Number, max :: Number, step :: Maybe Number }, uppercase :: Boolean, lowercase :: Boolean, digits :: Boolean, symbols :: Boolean } -> Number
entropyBits { length: len, uppercase, lowercase, digits, symbols } = len.current * log (toNumber (length (effectiveAlphabet { uppercase, lowercase, digits, symbols }))) / log 2.0

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
