module HereAndNow (Situation, clockPeriod, connectionLine, schemeLine, whenLine, whereLine) where

import Prelude ((<), (<>), const, show)

import Data.Number.Format (fixed, toStringWith)
import Data.Variant (match)

type Situation =
  { connection :: [ offline :: {}, online :: {} ]
  , position :: [ denied :: {}, located :: { lat :: Number, lng :: Number }, unknown :: {} ]
  , scheme :: [ dark :: {}, light :: {} ]
  , time :: { hours :: Int, minutes :: Int, seconds :: Int }
  }

clockPeriod :: { ms :: Number }
clockPeriod = { ms: 1000.0 }

whereLine :: Situation -> String
whereLine { position } = match
  { located: \{ lat, lng } -> "You are at " <> coordinate lat <> ", " <> coordinate lng <> "."
  , unknown: const "Locating you…"
  , denied: const "Your location is unavailable."
  } position

whenLine :: Situation -> String
whenLine { time } = "It is " <> padded time.hours <> ":" <> padded time.minutes <> ":" <> padded time.seconds <> "."

connectionLine :: Situation -> String
connectionLine { connection } = match { online: const "You are online.", offline: const "You are offline." } connection

schemeLine :: Situation -> String
schemeLine { scheme } = match { light: const "You prefer the light side.", dark: const "You prefer the dark side." } scheme

coordinate :: Number -> String
coordinate = toStringWith (fixed 2)

padded :: Int -> String
padded n = if n < 10 then "0" <> show n else show n
