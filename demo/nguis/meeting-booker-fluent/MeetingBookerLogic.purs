module MeetingBookerLogic (blankBooking, bookedLine, plan, ratedRoom, roomOf, seatsInRoom, seatsTaken) where

import Prelude ((<>), show, (/))

import Data.Int (round)
import Data.Ord (clamp)
import Data.String (trim)
import Data.Variant (match)
import Data.Variant.Case (caseText)

blankBooking :: { "Meeting title" :: String, "Room" :: [ chosen :: [ "Focus pod (4 seats)" :: {}, "Boardroom (12 seats)" :: {}, "Auditorium (40 seats)" :: {} ], unchosen :: {} ], "Duration (min)" :: [ chosen :: [ "15" :: {}, "30" :: {}, "60" :: {} ], unchosen :: {} ], "Attendees" :: { current :: Number, min :: Number, max :: Number, step :: [ discrete :: Number, continuous :: {} ] }, "Include a Teams link" :: Boolean }
blankBooking = { "Meeting title": "", "Room": .unchosen {}, "Duration (min)": .unchosen {}, "Attendees": { current: justTheOrganizer, min: justTheOrganizer, max: justTheOrganizer, step: .discrete 1.0 }, "Include a Teams link": false }

roomOf :: { "Room" :: [ chosen :: [ "Focus pod (4 seats)" :: {}, "Boardroom (12 seats)" :: {}, "Auditorium (40 seats)" :: {} ], unchosen :: {} ] } -> [ chosen :: [ "Focus pod (4 seats)" :: {}, "Boardroom (12 seats)" :: {}, "Auditorium (40 seats)" :: {} ], unchosen :: {} ]
roomOf { "Room": room } = room

seatsInRoom :: { "Room" :: [ chosen :: [ "Focus pod (4 seats)" :: {}, "Boardroom (12 seats)" :: {}, "Auditorium (40 seats)" :: {} ], unchosen :: {} ], "Attendees" :: { current :: Number, min :: Number, max :: Number, step :: [ discrete :: Number, continuous :: {} ] } } -> { "Room" :: [ chosen :: [ "Focus pod (4 seats)" :: {}, "Boardroom (12 seats)" :: {}, "Auditorium (40 seats)" :: {} ], unchosen :: {} ], "Attendees" :: { current :: Number, min :: Number, max :: Number, step :: [ discrete :: Number, continuous :: {} ] } }
seatsInRoom booking@{ "Room": room, "Attendees": seats } = match
  { chosen: \r -> booking { "Attendees" = seats { current = seatedIn r seats.current, max = roomCapacity r } }
  , unchosen: \_ -> booking
  } room

seatedIn :: [ "Focus pod (4 seats)" :: {}, "Boardroom (12 seats)" :: {}, "Auditorium (40 seats)" :: {} ] -> Number -> Number
seatedIn room n = clamp justTheOrganizer (roomCapacity room) n

plan :: { "Meeting title" :: String, "Room" :: [ chosen :: [ "Focus pod (4 seats)" :: {}, "Boardroom (12 seats)" :: {}, "Auditorium (40 seats)" :: {} ], unchosen :: {} ], "Duration (min)" :: [ chosen :: [ "15" :: {}, "30" :: {}, "60" :: {} ], unchosen :: {} ], "Attendees" :: { current :: Number, min :: Number, max :: Number, step :: [ discrete :: Number, continuous :: {} ] }, "Include a Teams link" :: Boolean } -> [ complete :: { titleText :: String, roomText :: String, durationText :: String, attendeesText :: String, onlineNote :: String }, incomplete :: {} ]
plan { "Meeting title": title, "Room": room, "Duration (min)": duration, "Attendees": seats, "Include a Teams link": online } = match
  { chosen: \r -> match
      { chosen: \d -> .complete { titleText: titleText title, roomText: roomText r, durationText: caseText d, attendeesText: headcount seats.current, onlineNote: onlineNote online }
      , unchosen: \_ -> .incomplete {}
      } duration
  , unchosen: \_ -> .incomplete {}
  } room

bookedLine :: { titleText :: String, roomText :: String, durationText :: String, attendeesText :: String, onlineNote :: String } -> String
bookedLine { titleText: title, roomText: room, durationText: duration } =
  "Booked: " <> title <> " — " <> room <> " for " <> duration <> " min"

headcount :: Number -> String
headcount attendees = show (round attendees)

onlineNote :: Boolean -> String
onlineNote online = if online then ", with a Teams link" else ""

titleText :: String -> String
titleText title = case trim title of
  "" -> "Untitled meeting"
  name -> name

roomText :: [ "Focus pod (4 seats)" :: {}, "Boardroom (12 seats)" :: {}, "Auditorium (40 seats)" :: {} ] -> String
roomText = match { "Focus pod (4 seats)": \_ -> "focus pod", "Boardroom (12 seats)": \_ -> "boardroom", "Auditorium (40 seats)": \_ -> "auditorium" }

ratedRoom :: { "Room" :: [ chosen :: [ "Focus pod (4 seats)" :: {}, "Boardroom (12 seats)" :: {}, "Auditorium (40 seats)" :: {} ], unchosen :: {} ] } -> [ rated :: { rating :: Number }, unrated :: {} ]
ratedRoom { "Room": room } = match { chosen: \r -> .rated { rating: roomRating r }, unchosen: \_ -> .unrated {} } room

roomRating :: [ "Focus pod (4 seats)" :: {}, "Boardroom (12 seats)" :: {}, "Auditorium (40 seats)" :: {} ] -> Number
roomRating = match { "Focus pod (4 seats)": \_ -> 4.5, "Boardroom (12 seats)": \_ -> 3.5, "Auditorium (40 seats)": \_ -> 4.0 }

seatsTaken :: { "Room" :: [ chosen :: [ "Focus pod (4 seats)" :: {}, "Boardroom (12 seats)" :: {}, "Auditorium (40 seats)" :: {} ], unchosen :: {} ], "Attendees" :: { current :: Number, min :: Number, max :: Number, step :: [ discrete :: Number, continuous :: {} ] } } -> [ seated :: { occupancy :: Number }, unseated :: {} ]
seatsTaken { "Room": room, "Attendees": seats } = match { chosen: \r -> .seated { occupancy: seats.current / roomCapacity r }, unchosen: \_ -> .unseated {} } room

roomCapacity :: [ "Focus pod (4 seats)" :: {}, "Boardroom (12 seats)" :: {}, "Auditorium (40 seats)" :: {} ] -> Number
roomCapacity = match { "Focus pod (4 seats)": \_ -> 4.0, "Boardroom (12 seats)": \_ -> 12.0, "Auditorium (40 seats)": \_ -> 40.0 }

justTheOrganizer :: Number
justTheOrganizer = 1.0
