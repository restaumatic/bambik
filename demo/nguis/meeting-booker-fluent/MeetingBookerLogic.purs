module MeetingBookerLogic (blankBooking, bookedLine, completePlan, headcount, onlineNote, ratedRoom, roomChoice, roomText, seatsInRoom, seatsTaken, titleText) where

import Prelude ((<>), show, (/), (<$>), (<*>))

import Data.Int (round)
import Data.Maybe (Maybe(..))
import Data.Ord (clamp)
import Data.String (trim)
import Data.Variant (match)
import Data.Variant.Case (caseText)

blankBooking :: { "Meeting title" :: String, "Room" :: Maybe [ "Focus pod (4 seats)" :: {}, "Boardroom (12 seats)" :: {}, "Auditorium (40 seats)" :: {} ], "Duration (min)" :: Maybe [ "15" :: {}, "30" :: {}, "60" :: {} ], "Attendees" :: { current :: Number, min :: Number, max :: Number, step :: Maybe Number }, "Include a Teams link" :: Boolean }
blankBooking = { "Meeting title": "", "Room": Nothing, "Duration (min)": Nothing, "Attendees": { current: justTheOrganizer, min: justTheOrganizer, max: justTheOrganizer, step: Just 1.0 }, "Include a Teams link": false }

roomChoice :: { "Room" :: Maybe [ "Focus pod (4 seats)" :: {}, "Boardroom (12 seats)" :: {}, "Auditorium (40 seats)" :: {} ] } -> [ chosen :: {}, unchosen :: {} ]
roomChoice { "Room": Just _ } = .chosen {}
roomChoice { "Room": Nothing } = .unchosen {}

seatsInRoom :: { "Room" :: Maybe [ "Focus pod (4 seats)" :: {}, "Boardroom (12 seats)" :: {}, "Auditorium (40 seats)" :: {} ], "Attendees" :: { current :: Number, min :: Number, max :: Number, step :: Maybe Number } } -> { "Room" :: Maybe [ "Focus pod (4 seats)" :: {}, "Boardroom (12 seats)" :: {}, "Auditorium (40 seats)" :: {} ], "Attendees" :: { current :: Number, min :: Number, max :: Number, step :: Maybe Number } }
seatsInRoom { "Room": Just room, "Attendees": seats } = { "Room": Just room, "Attendees": seats { current = seatedIn room seats.current, max = roomCapacity room } }
seatsInRoom booking = booking

seatedIn :: [ "Focus pod (4 seats)" :: {}, "Boardroom (12 seats)" :: {}, "Auditorium (40 seats)" :: {} ] -> Number -> Number
seatedIn room n = clamp justTheOrganizer (roomCapacity room) n

completePlan :: { "Meeting title" :: String, "Room" :: Maybe [ "Focus pod (4 seats)" :: {}, "Boardroom (12 seats)" :: {}, "Auditorium (40 seats)" :: {} ], "Duration (min)" :: Maybe [ "15" :: {}, "30" :: {}, "60" :: {} ], "Attendees" :: { current :: Number, min :: Number, max :: Number, step :: Maybe Number }, "Include a Teams link" :: Boolean } -> Maybe { "Meeting title" :: String, "Room" :: [ "Focus pod (4 seats)" :: {}, "Boardroom (12 seats)" :: {}, "Auditorium (40 seats)" :: {} ], "Duration (min)" :: [ "15" :: {}, "30" :: {}, "60" :: {} ], attendees :: Number, "Include a Teams link" :: Boolean }
completePlan { "Meeting title": title, "Room": room, "Duration (min)": duration, "Attendees": seats, "Include a Teams link": online } =
  (\r d -> { "Meeting title": title, "Room": r, "Duration (min)": d, attendees: seats.current, "Include a Teams link": online }) <$> room <*> duration

bookedLine :: { "Meeting title" :: String, "Room" :: [ "Focus pod (4 seats)" :: {}, "Boardroom (12 seats)" :: {}, "Auditorium (40 seats)" :: {} ], "Duration (min)" :: [ "15" :: {}, "30" :: {}, "60" :: {} ], attendees :: Number, "Include a Teams link" :: Boolean } -> String
bookedLine { "Meeting title": title, "Room": room, "Duration (min)": duration } =
  "Booked: " <> titleText title <> " — " <> roomText room <> " for " <> caseText duration <> " min"

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

ratedRoom :: { "Room" :: Maybe [ "Focus pod (4 seats)" :: {}, "Boardroom (12 seats)" :: {}, "Auditorium (40 seats)" :: {} ] } -> Maybe { rating :: Number }
ratedRoom { "Room": room } = (\r -> { rating: roomRating r }) <$> room

roomRating :: [ "Focus pod (4 seats)" :: {}, "Boardroom (12 seats)" :: {}, "Auditorium (40 seats)" :: {} ] -> Number
roomRating = match { "Focus pod (4 seats)": \_ -> 4.5, "Boardroom (12 seats)": \_ -> 3.5, "Auditorium (40 seats)": \_ -> 4.0 }

seatsTaken :: { "Room" :: Maybe [ "Focus pod (4 seats)" :: {}, "Boardroom (12 seats)" :: {}, "Auditorium (40 seats)" :: {} ], "Attendees" :: { current :: Number, min :: Number, max :: Number, step :: Maybe Number } } -> Maybe { occupancy :: Number }
seatsTaken { "Room": room, "Attendees": seats } = (\r -> { occupancy: seats.current / roomCapacity r }) <$> room

roomCapacity :: [ "Focus pod (4 seats)" :: {}, "Boardroom (12 seats)" :: {}, "Auditorium (40 seats)" :: {} ] -> Number
roomCapacity = match { "Focus pod (4 seats)": \_ -> 4.0, "Boardroom (12 seats)": \_ -> 12.0, "Auditorium (40 seats)": \_ -> 40.0 }

justTheOrganizer :: Number
justTheOrganizer = 1.0
