module PasswordGeneratorViewModel (passwordText, samplePassword, strengthLine, strongMixRecipe) where

import Prelude (bind, otherwise, pure, (*), (-), (/), (<), (<>))

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

strongMixRecipe
  :: { "Digits" :: Boolean
     , "Length" :: { current :: Number
                   , max :: Number
                   , min :: Number
                   , step :: [ continuous :: {}, discrete :: Number ]
                   }
     , "Lowercase letters" :: Boolean
     , "Symbols" :: Boolean
     , "Uppercase letters" :: Boolean
     , password :: String
     }
strongMixRecipe =
  { "Length": passwordLengths 16.0
  , "Uppercase letters": true
  , "Lowercase letters": true
  , "Digits": true
  , "Symbols": false
  , password: ""
  }

strengthLine
  :: { "Digits" :: Boolean
     , "Length" :: { current :: Number
                   , max :: Number
                   , min :: Number
                   , step :: [ continuous :: {}, discrete :: Number ]
                   }
     , "Lowercase letters" :: Boolean
     , "Symbols" :: Boolean
     , "Uppercase letters" :: Boolean
     , password :: String
     }
  -> String
strengthLine r = "Strength: " <> strengthGrade (entropyBits r)

passwordLengths :: Number -> { current :: Number, min :: Number, max :: Number, step :: [ discrete :: Number, continuous :: {} ] }
passwordLengths n = { current: n, min: 8.0, max: 64.0, step: .discrete 1.0 }

samplePassword
  :: { "Digits" :: Boolean
     , "Length" :: { current :: Number
                   , max :: Number
                   , min :: Number
                   , step :: [ continuous :: {}, discrete :: Number ]
                   }
     , "Lowercase letters" :: Boolean
     , "Symbols" :: Boolean
     , "Uppercase letters" :: Boolean
     , password :: String
     }
  -> Aff [ "Password generated" :: { "Digits" :: Boolean
                                   , "Length" :: { current :: Number
                                                 , max :: Number
                                                 , min :: Number
                                                 , step :: [ continuous :: {}, discrete :: Number ]
                                                 }
                                   , "Lowercase letters" :: Boolean
                                   , "Symbols" :: Boolean
                                   , "Uppercase letters" :: Boolean
                                   , password :: String
                                   }
         ]
samplePassword recipe@{ "Length": length, "Uppercase letters": uppercase, "Lowercase letters": lowercase, "Digits": digits, "Symbols": symbols } = liftEffect do
  let alphabet = effectiveAlphabet { "Uppercase letters": uppercase, "Lowercase letters": lowercase, "Digits": digits, "Symbols": symbols }
  chars <- sequence (replicate (round length.current) (randomCharacter alphabet))
  pure (."Password generated" (rememberPassword (fromCharArray chars) recipe))

randomCharacter :: Array Char -> Effect Char
randomCharacter alphabet = do
  i <- randomInt 0 (length alphabet - 1)
  pure (fromMaybe 'a' (index alphabet i))

passwordText
  :: { "Digits" :: Boolean
     , "Length" :: { current :: Number
                   , max :: Number
                   , min :: Number
                   , step :: [ continuous :: {}, discrete :: Number ]
                   }
     , "Lowercase letters" :: Boolean
     , "Symbols" :: Boolean
     , "Uppercase letters" :: Boolean
     , password :: String
     }
  -> String
passwordText { password } = password

rememberPassword :: String -> { "Digits" :: Boolean, "Length" :: { current :: Number, max :: Number, min :: Number, step :: [ continuous :: {}, discrete :: Number ] }, "Lowercase letters" :: Boolean, "Symbols" :: Boolean, "Uppercase letters" :: Boolean, password :: String } -> { "Digits" :: Boolean, "Length" :: { current :: Number, max :: Number, min :: Number, step :: [ continuous :: {}, discrete :: Number ] }, "Lowercase letters" :: Boolean, "Symbols" :: Boolean, "Uppercase letters" :: Boolean, password :: String }
rememberPassword password recipe = recipe { password = password }

effectiveAlphabet :: forall r1. { "Uppercase letters" :: Boolean, "Lowercase letters" :: Boolean, "Digits" :: Boolean, "Symbols" :: Boolean | r1 } -> Array Char
effectiveAlphabet { "Uppercase letters": uppercase, "Lowercase letters": lowercase, "Digits": digits, "Symbols": symbols } =
  let chosen = (if uppercase then uppercaseLetters else [])
            <> (if lowercase then lowercaseLetters else [])
            <> (if digits then digitCharacters else [])
            <> (if symbols then symbolCharacters else [])
  in if null chosen then lowercaseLetters else chosen

entropyBits :: forall r1. { "Length" :: { current :: Number, min :: Number, max :: Number, step :: [ discrete :: Number, continuous :: {} ] }, "Uppercase letters" :: Boolean, "Lowercase letters" :: Boolean, "Digits" :: Boolean, "Symbols" :: Boolean | r1 } -> Number
entropyBits { "Length": len, "Uppercase letters": uppercase, "Lowercase letters": lowercase, "Digits": digits, "Symbols": symbols } = len.current * log (toNumber (length (effectiveAlphabet { "Uppercase letters": uppercase, "Lowercase letters": lowercase, "Digits": digits, "Symbols": symbols }))) / log 2.0

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
