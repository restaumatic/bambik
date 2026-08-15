module MeetingBookerLogic (blankBooking, bookedLine, chooseSeats, completePlan, durationText, headcount, onlineNote, ratedRoom, roomText, seatsFor, seatsTaken, titleText) where

import Prelude (show, (/), (<$>), (<*>), (<>))

import Data.Int (round)
import Data.Maybe (Maybe(..))
import Data.Ord (clamp)
import Data.String (trim)
import Data.Variant (match)

blankBooking :: { "Meeting title" :: String, "Room" :: Maybe [ "Focus pod (4 seats)" :: {}, "Boardroom (12 seats)" :: {}, "Auditorium (40 seats)" :: {} ], "Duration" :: Maybe [ "15 min" :: {}, "30 min" :: {}, "60 min" :: {} ], attendees :: Number, "Include a Teams link" :: Boolean }
blankBooking = { "Meeting title": "", "Room": Nothing, "Duration": Nothing, attendees: justTheOrganizer, "Include a Teams link": false }

seatsFor :: { "Room" :: Maybe [ "Focus pod (4 seats)" :: {}, "Boardroom (12 seats)" :: {}, "Auditorium (40 seats)" :: {} ], attendees :: Number } -> Maybe { "Attendees" :: { current :: Number, min :: Number, max :: Number, step :: Maybe Number } }
seatsFor { "Room": room, attendees } = (\r -> { "Attendees": { current: seatedIn r attendees, min: justTheOrganizer, max: roomCapacity r, step: Just 1.0 } }) <$> room

chooseSeats :: { "Attendees" :: { current :: Number, min :: Number, max :: Number, step :: Maybe Number } } -> { attendees :: Number }
chooseSeats { "Attendees": seats } = { attendees: seats.current }

seatedIn :: [ "Focus pod (4 seats)" :: {}, "Boardroom (12 seats)" :: {}, "Auditorium (40 seats)" :: {} ] -> Number -> Number
seatedIn room n = clamp justTheOrganizer (roomCapacity room) n

completePlan :: { "Meeting title" :: String, "Room" :: Maybe [ "Focus pod (4 seats)" :: {}, "Boardroom (12 seats)" :: {}, "Auditorium (40 seats)" :: {} ], "Duration" :: Maybe [ "15 min" :: {}, "30 min" :: {}, "60 min" :: {} ], attendees :: Number, "Include a Teams link" :: Boolean } -> Maybe { "Meeting title" :: String, "Room" :: [ "Focus pod (4 seats)" :: {}, "Boardroom (12 seats)" :: {}, "Auditorium (40 seats)" :: {} ], "Duration" :: [ "15 min" :: {}, "30 min" :: {}, "60 min" :: {} ], attendees :: Number, "Include a Teams link" :: Boolean }
completePlan { "Meeting title": title, "Room": room, "Duration": duration, attendees, "Include a Teams link": online } =
  (\r d -> { "Meeting title": title, "Room": r, "Duration": d, attendees: seatedIn r attendees, "Include a Teams link": online }) <$> room <*> duration

bookedLine :: { "Meeting title" :: String, "Room" :: [ "Focus pod (4 seats)" :: {}, "Boardroom (12 seats)" :: {}, "Auditorium (40 seats)" :: {} ], "Duration" :: [ "15 min" :: {}, "30 min" :: {}, "60 min" :: {} ], attendees :: Number, "Include a Teams link" :: Boolean } -> String
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

roomText :: [ "Focus pod (4 seats)" :: {}, "Boardroom (12 seats)" :: {}, "Auditorium (40 seats)" :: {} ] -> String
roomText = match { "Focus pod (4 seats)": \_ -> "focus pod", "Boardroom (12 seats)": \_ -> "boardroom", "Auditorium (40 seats)": \_ -> "auditorium" }

durationText :: [ "15 min" :: {}, "30 min" :: {}, "60 min" :: {} ] -> String
durationText = match { "15 min": \_ -> "15 min", "30 min": \_ -> "30 min", "60 min": \_ -> "60 min" }

ratedRoom :: { "Room" :: Maybe [ "Focus pod (4 seats)" :: {}, "Boardroom (12 seats)" :: {}, "Auditorium (40 seats)" :: {} ] } -> Maybe { rating :: Number }
ratedRoom { "Room": room } = (\r -> { rating: roomRating r }) <$> room

roomRating :: [ "Focus pod (4 seats)" :: {}, "Boardroom (12 seats)" :: {}, "Auditorium (40 seats)" :: {} ] -> Number
roomRating = match { "Focus pod (4 seats)": \_ -> 4.5, "Boardroom (12 seats)": \_ -> 3.5, "Auditorium (40 seats)": \_ -> 4.0 }

seatsTaken :: { "Room" :: Maybe [ "Focus pod (4 seats)" :: {}, "Boardroom (12 seats)" :: {}, "Auditorium (40 seats)" :: {} ], attendees :: Number } -> Maybe { occupancy :: Number }
seatsTaken { "Room": room, attendees } = (\r -> { occupancy: seatedIn r attendees / roomCapacity r }) <$> room

roomCapacity :: [ "Focus pod (4 seats)" :: {}, "Boardroom (12 seats)" :: {}, "Auditorium (40 seats)" :: {} ] -> Number
roomCapacity = match { "Focus pod (4 seats)": \_ -> 4.0, "Boardroom (12 seats)": \_ -> 12.0, "Auditorium (40 seats)": \_ -> 40.0 }

justTheOrganizer :: Number
justTheOrganizer = 1.0
