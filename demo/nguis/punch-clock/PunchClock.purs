module PunchClock (Clocked, Entry, Punch, Shift, ShiftRow, Time, clockLine, clockPeriod, emptyShift, punchIn, punchLine, punches) where

import Prelude ((*), (+), (-), (/), (<), (<>), (==), map, mod, show)

import Data.Array (cons, length)
import Data.String (trim)

type Shift = { | ShiftRow () }

type Clocked = { | ShiftRow ( time :: Time ) }

type ShiftRow r = ( "Name" :: String, entries :: Array Entry | r )

type Entry = { at :: Time, name :: String, number :: Int }

type Punch = { ago :: Int, at :: Time, name :: String, number :: Int }

type Time = { hours :: Int, minutes :: Int, seconds :: Int }

clockPeriod :: { ms :: Number }
clockPeriod = { ms: 1000.0 }

emptyShift :: Shift
emptyShift = { "Name": "", entries: [] }

punchIn :: { event :: Clocked, model :: Shift } -> Shift
punchIn { event: { time }, model } =
  if name == "" then model
  else model { "Name" = "", entries = cons { at: time, name, number: length model.entries + 1 } model.entries }
  where
  name = trim model."Name"

punches :: Clocked -> Array Punch
punches { entries, time } = map (\{ at, name, number } -> { ago: secondsBetween at time, at, name, number }) entries

clockLine :: Clocked -> String
clockLine { time } = "It is " <> timeText time

punchLine :: Punch -> String
punchLine { ago, at, name } = name <> " · in at " <> timeText at <> " · " <> agoText ago

timeText :: Time -> String
timeText { hours, minutes, seconds } = twoDigits hours <> ":" <> twoDigits minutes <> ":" <> twoDigits seconds
  where
  twoDigits n = if n < 10 then "0" <> show n else show n

agoText :: Int -> String
agoText seconds =
  if seconds < 5 then "just now"
  else if seconds < 60 then show seconds <> " s ago"
  else if seconds < 3600 then show (seconds / 60) <> " min ago"
  else show (seconds / 3600) <> " h ago"

secondsBetween :: Time -> Time -> Int
secondsBetween earlier later = (ofDay later - ofDay earlier + day) `mod` day
  where
  ofDay { hours, minutes, seconds } = hours * 3600 + minutes * 60 + seconds
  day = 86400
