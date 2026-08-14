module MeetingBookerLogic (blankBooking, bookedLine, chooseSeats, completePlan, durationText, headcount, onlineNote, ratedRoom, roomText, seatsFor, seatsTaken, titleText) where

import Prelude (show, (/), (<$>), (<*>), (<>))

import Data.Int (round)
import Data.Maybe (Maybe(..))
import Data.Ord (clamp)
import Data.String (trim)
import Data.Variant (match)

blankBooking :: { title :: String, room :: Maybe [ focusPod :: {}, boardroom :: {}, auditorium :: {} ], duration :: Maybe [ quarter :: {}, half :: {}, hour :: {} ], attendees :: Number, online :: Boolean }
blankBooking = { title: "", room: Nothing, duration: Nothing, attendees: justTheOrganizer, online: false }

seatsFor :: { room :: Maybe [ focusPod :: {}, boardroom :: {}, auditorium :: {} ], attendees :: Number } -> Maybe { seats :: { current :: Number, min :: Number, max :: Number, step :: Maybe Number } }
seatsFor { room, attendees } = (\r -> { seats: { current: seatedIn r attendees, min: justTheOrganizer, max: roomCapacity r, step: Just 1.0 } }) <$> room

chooseSeats :: { seats :: { current :: Number, min :: Number, max :: Number, step :: Maybe Number } } -> { attendees :: Number }
chooseSeats { seats } = { attendees: seats.current }

seatedIn :: [ focusPod :: {}, boardroom :: {}, auditorium :: {} ] -> Number -> Number
seatedIn room n = clamp justTheOrganizer (roomCapacity room) n

completePlan :: { title :: String, room :: Maybe [ focusPod :: {}, boardroom :: {}, auditorium :: {} ], duration :: Maybe [ quarter :: {}, half :: {}, hour :: {} ], attendees :: Number, online :: Boolean } -> Maybe { title :: String, room :: [ focusPod :: {}, boardroom :: {}, auditorium :: {} ], duration :: [ quarter :: {}, half :: {}, hour :: {} ], attendees :: Number, online :: Boolean }
completePlan { title, room, duration, attendees, online } =
  (\r d -> { title, room: r, duration: d, attendees: seatedIn r attendees, online }) <$> room <*> duration

bookedLine :: { title :: String, room :: [ focusPod :: {}, boardroom :: {}, auditorium :: {} ], duration :: [ quarter :: {}, half :: {}, hour :: {} ], attendees :: Number, online :: Boolean } -> String
bookedLine { title, room, duration } =
  "Booked: " <> titleText title <> " — " <> roomText room <> " for " <> durationText duration

headcount :: Number -> String
headcount attendees = show (round attendees)

onlineNote :: { online :: Boolean } -> String
onlineNote { online } = if online then ", with a Teams link" else ""

titleText :: String -> String
titleText title = case trim title of
  "" -> "Untitled meeting"
  name -> name

roomText :: [ focusPod :: {}, boardroom :: {}, auditorium :: {} ] -> String
roomText = match { focusPod: \_ -> "focus pod", boardroom: \_ -> "boardroom", auditorium: \_ -> "auditorium" }

durationText :: [ quarter :: {}, half :: {}, hour :: {} ] -> String
durationText = match { quarter: \_ -> "15 min", half: \_ -> "30 min", hour: \_ -> "60 min" }

ratedRoom :: { room :: Maybe [ focusPod :: {}, boardroom :: {}, auditorium :: {} ] } -> Maybe { rating :: Number }
ratedRoom { room } = (\r -> { rating: roomRating r }) <$> room

roomRating :: [ focusPod :: {}, boardroom :: {}, auditorium :: {} ] -> Number
roomRating = match { focusPod: \_ -> 4.5, boardroom: \_ -> 3.5, auditorium: \_ -> 4.0 }

seatsTaken :: { room :: Maybe [ focusPod :: {}, boardroom :: {}, auditorium :: {} ], attendees :: Number } -> Maybe { occupancy :: Number }
seatsTaken { room, attendees } = (\r -> { occupancy: seatedIn r attendees / roomCapacity r }) <$> room

roomCapacity :: [ focusPod :: {}, boardroom :: {}, auditorium :: {} ] -> Number
roomCapacity = match { focusPod: \_ -> 4.0, boardroom: \_ -> 12.0, auditorium: \_ -> 40.0 }

justTheOrganizer :: Number
justTheOrganizer = 1.0
