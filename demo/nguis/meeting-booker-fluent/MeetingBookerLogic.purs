module MeetingBookerLogic (blankBooking, bookedLine, chooseSeats, completePlan, durationText, headcount, onlineNote, ratedRoom, roomText, seatsFor, seatsTaken, titleText) where

import Prelude (show, (/), (<$>), (<*>), (<>))

import Data.Int (round)
import Data.Maybe (Maybe(..))
import Data.Ord (clamp)
import Data.String (trim)
import Data.Variant (match)

blankBooking :: { "Meeting title" :: String, "Room" :: Maybe [ focusPod :: {}, boardroom :: {}, auditorium :: {} ], "Duration" :: Maybe [ quarter :: {}, half :: {}, hour :: {} ], attendees :: Number, "Include a Teams link" :: Boolean }
blankBooking = { "Meeting title": "", "Room": Nothing, "Duration": Nothing, attendees: justTheOrganizer, "Include a Teams link": false }

seatsFor :: { "Room" :: Maybe [ focusPod :: {}, boardroom :: {}, auditorium :: {} ], attendees :: Number } -> Maybe { "Attendees" :: { current :: Number, min :: Number, max :: Number, step :: Maybe Number } }
seatsFor { "Room": room, attendees } = (\r -> { "Attendees": { current: seatedIn r attendees, min: justTheOrganizer, max: roomCapacity r, step: Just 1.0 } }) <$> room

chooseSeats :: { "Attendees" :: { current :: Number, min :: Number, max :: Number, step :: Maybe Number } } -> { attendees :: Number }
chooseSeats { "Attendees": seats } = { attendees: seats.current }

seatedIn :: [ focusPod :: {}, boardroom :: {}, auditorium :: {} ] -> Number -> Number
seatedIn room n = clamp justTheOrganizer (roomCapacity room) n

completePlan :: { "Meeting title" :: String, "Room" :: Maybe [ focusPod :: {}, boardroom :: {}, auditorium :: {} ], "Duration" :: Maybe [ quarter :: {}, half :: {}, hour :: {} ], attendees :: Number, "Include a Teams link" :: Boolean } -> Maybe { "Meeting title" :: String, "Room" :: [ focusPod :: {}, boardroom :: {}, auditorium :: {} ], "Duration" :: [ quarter :: {}, half :: {}, hour :: {} ], attendees :: Number, "Include a Teams link" :: Boolean }
completePlan { "Meeting title": title, "Room": room, "Duration": duration, attendees, "Include a Teams link": online } =
  (\r d -> { "Meeting title": title, "Room": r, "Duration": d, attendees: seatedIn r attendees, "Include a Teams link": online }) <$> room <*> duration

bookedLine :: { "Meeting title" :: String, "Room" :: [ focusPod :: {}, boardroom :: {}, auditorium :: {} ], "Duration" :: [ quarter :: {}, half :: {}, hour :: {} ], attendees :: Number, "Include a Teams link" :: Boolean } -> String
bookedLine { "Meeting title": title, "Room": room, "Duration": duration } =
  "Booked: " <> titleText title <> " — " <> roomText room <> " for " <> durationText duration

headcount :: Number -> String
headcount attendees = show (round attendees)

onlineNote :: { "Include a Teams link" :: Boolean } -> String
onlineNote { "Include a Teams link": online } = if online then ", with a Teams link" else ""

titleText :: String -> String
titleText title = case trim title of
  "" -> "Untitled meeting"
  name -> name

roomText :: [ focusPod :: {}, boardroom :: {}, auditorium :: {} ] -> String
roomText = match { focusPod: \_ -> "focus pod", boardroom: \_ -> "boardroom", auditorium: \_ -> "auditorium" }

durationText :: [ quarter :: {}, half :: {}, hour :: {} ] -> String
durationText = match { quarter: \_ -> "15 min", half: \_ -> "30 min", hour: \_ -> "60 min" }

ratedRoom :: { "Room" :: Maybe [ focusPod :: {}, boardroom :: {}, auditorium :: {} ] } -> Maybe { rating :: Number }
ratedRoom { "Room": room } = (\r -> { rating: roomRating r }) <$> room

roomRating :: [ focusPod :: {}, boardroom :: {}, auditorium :: {} ] -> Number
roomRating = match { focusPod: \_ -> 4.5, boardroom: \_ -> 3.5, auditorium: \_ -> 4.0 }

seatsTaken :: { "Room" :: Maybe [ focusPod :: {}, boardroom :: {}, auditorium :: {} ], attendees :: Number } -> Maybe { occupancy :: Number }
seatsTaken { "Room": room, attendees } = (\r -> { occupancy: seatedIn r attendees / roomCapacity r }) <$> room

roomCapacity :: [ focusPod :: {}, boardroom :: {}, auditorium :: {} ] -> Number
roomCapacity = match { focusPod: \_ -> 4.0, boardroom: \_ -> 12.0, auditorium: \_ -> 40.0 }

justTheOrganizer :: Number
justTheOrganizer = 1.0
