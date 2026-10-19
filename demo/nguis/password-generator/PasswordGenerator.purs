module PasswordGenerator (Quantity, Recipe, characterKinds, samplePassword, strengthLine, strongMixRecipe) where

import Prelude (bind, otherwise, pure, show, (*), (-), (/), (<), (<>))

import Data.Array (index, length, null, replicate)
import Data.Int (round, toNumber)
import Data.Maybe (fromMaybe)
import Data.Number (log)
import Data.String.CodeUnits (fromCharArray, toCharArray)
import Data.Traversable (sequence)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Random (randomInt)

type Recipe =
  { "Length" :: Quantity
  , "Uppercase letters" :: Boolean
  , "Lowercase letters" :: Boolean
  , "Digits" :: Boolean
  , "Symbols" :: Boolean
  , password :: String
  }

type Quantity =
  { current :: Number
  , min :: Number
  , max :: Number
  , step :: [ discrete :: Number, continuous :: {} ]
  }

strongMixRecipe :: Recipe
strongMixRecipe =
  { "Length": { current: 16.0, min: 8.0, max: 64.0, step: .discrete 1.0 }
  , "Uppercase letters": true
  , "Lowercase letters": true
  , "Digits": true
  , "Symbols": false
  , password: ""
  }

strengthLine :: Recipe -> String
strengthLine r
  | null (effectiveAlphabet r) = "Pick at least one kind of character"
  | otherwise = "Strength: " <> strengthGrade (entropyBits r) <> " (" <> show (round r."Length".current) <> " characters)"

characterKinds :: Recipe -> [ chosen :: {}, none :: {} ]
characterKinds r = if null (effectiveAlphabet r) then .none {} else .chosen {}

samplePassword :: Recipe -> Aff [ "Password generated" :: Recipe ]
samplePassword recipe = liftEffect do
  chars <- sequence (replicate (round recipe."Length".current) (randomCharacter (effectiveAlphabet recipe)))
  pure (."Password generated" recipe { password = fromCharArray chars })

randomCharacter :: Array Char -> Effect Char
randomCharacter alphabet = do
  i <- randomInt 0 (length alphabet - 1)
  pure (fromMaybe 'a' (index alphabet i))

effectiveAlphabet :: Recipe -> Array Char
effectiveAlphabet r =
  (if r."Uppercase letters" then uppercaseLetters else [])
    <> (if r."Lowercase letters" then lowercaseLetters else [])
    <> (if r."Digits" then digitCharacters else [])
    <> (if r."Symbols" then symbolCharacters else [])

entropyBits :: Recipe -> Number
entropyBits r = r."Length".current * log (toNumber (length (effectiveAlphabet r))) / log 2.0

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
