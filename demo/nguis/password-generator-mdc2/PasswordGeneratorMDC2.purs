module PasswordGeneratorMDC2 (passwordGeneratorMDC2) where

import Prelude (identity, (#), ($), (*), (-), (/), (<), (<>), (>>>), Unit, bind, otherwise, pure)

import Data.Array (index, length, null, replicate)
import Data.Int (round, toNumber)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Number (log)
import Data.String.CodeUnits (fromCharArray, toCharArray)
import Data.Traversable (sequence)
import Data.Variant (match)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Random (randomInt)
import PUI (action, asCase, asField, completed, forField, mvu, onCase, projected, tapped, updated)
import PUI.HTML (attr, body, div, staticText, text)
import PUI.MDC2 (body2, button, card, elevation20, indeterminateLinearProgress, slider, toggleSwitch)
import Data.Profunctor.Row.RecordToRecord as RecordToRecord
import QualifiedDo.Semigroupoid as Semigroupoid

passwordGeneratorMDC2 :: Effect Unit
passwordGeneratorMDC2 =
  body $
    elevation20 $
      card { caption: "Password Generator" } $ ( Semigroupoid.do
          ( RecordToRecord.do
              slider { label: "Length" } # asField @"length"
              toggleSwitch { label: "Uppercase letters" } # asField @"uppercase"
              toggleSwitch { label: "Lowercase letters" } # asField @"lowercase"
              toggleSwitch { label: "Digits" } # asField @"digits"
              toggleSwitch { label: "Symbols" } # asField @"symbols") # completed
          body2 ( RecordToRecord.do
              staticText "Strength: "
              text # projected strengthText ) # tapped
          div >>> attr "style" "font-family: monospace; font-size: 1.2rem; word-break: break-all; min-height: 1.6rem; margin: 8px 0;" >>> attr "id" "password" $
            text # forField @"password" identity # tapped
          ( Semigroupoid.do
              button { label: "Generate" } # asCase @"generate"
              indeterminateLinearProgress # action samplePassword # onCase @"generate") # updated (match { generated: rememberPassword })
      ) # mvu strongMixRecipe

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
