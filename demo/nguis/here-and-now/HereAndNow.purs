module HereAndNow (Position, Time, clockPeriod, connectionLine, languageLine, motionLine, pointerLine, schemeLine, whenLine, whereLine, windowLine) where

import Prelude ((<), (<>), const, show)

import Data.Number.Format (fixed, toStringWith)
import Data.Variant (match)

type Position = [ denied :: {}, located :: { lat :: Number, lng :: Number }, unknown :: {} ]

type Time = { hours :: Int, minutes :: Int, seconds :: Int }

clockPeriod :: { ms :: Number }
clockPeriod = { ms: 1000.0 }

whereLine :: { position :: Position } -> String
whereLine { position } = match
  { located: \{ lat, lng } -> "You are at " <> coordinate lat <> ", " <> coordinate lng <> "."
  , unknown: const "Locating you…"
  , denied: const "Your location is unavailable."
  } position

whenLine :: { time :: Time, zone :: String } -> String
whenLine { time, zone } = "It is " <> padded time.hours <> ":" <> padded time.minutes <> ":" <> padded time.seconds <> " in " <> zone <> "."

connectionLine :: { connection :: [ offline :: {}, online :: {} ] } -> String
connectionLine { connection } = match { online: const "You are online.", offline: const "You are offline." } connection

schemeLine :: { scheme :: [ dark :: {}, light :: {} ] } -> String
schemeLine { scheme } = match { light: const "You prefer the light side.", dark: const "You prefer the dark side." } scheme

motionLine :: { motion :: [ noPreference :: {}, reduce :: {} ] } -> String
motionLine { motion } = match { reduce: const "You prefer less motion.", noPreference: const "Motion does not bother you." } motion

pointerLine :: { pointer :: [ coarse :: {}, fine :: {}, none :: {} ] } -> String
pointerLine { pointer } = match
  { coarse: const "You point with a finger."
  , fine: const "You point with a mouse or a pen."
  , none: const "You have nothing to point with."
  } pointer

windowLine :: { window :: { height :: Int, width :: Int } } -> String
windowLine { window } = "Your window is " <> show window.width <> " × " <> show window.height <> " pixels."

languageLine :: { language :: String } -> String
languageLine { language } = "Your browser speaks " <> language <> "."

coordinate :: Number -> String
coordinate = toStringWith (fixed 2)

padded :: Int -> String
padded n = if n < 10 then "0" <> show n else show n
