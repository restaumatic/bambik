module MeetingBookerViewModel (blankBooking, bookedLine, plan, planLine, ratedRoom, roomRatingCaption, roomStars, seatOccupancy, seatsInRoom, seatsTaken, seatsTakenCaption) where

import Prelude ((<>), show, (/))

import Data.Int (round)
import Data.Ord (clamp)
import Data.String (trim)
import Data.Variant (match)
import Data.Variant.Case (caseText)

blankBooking :: { "Attendees" :: { current :: Number, max :: Number, min :: Number, step :: [ continuous :: {}, discrete :: Number ] }, "Catering" :: [ none :: {}, ordered :: [ "coffee and pastries" :: {}, "sandwich lunch" :: {} ] ], "Duration (min)" :: [ chosen :: [ "15" :: {}, "30" :: {}, "60" :: {} ], unchosen :: {} ], "Include a Teams link" :: Boolean, "Meeting title" :: String, "Room" :: [ chosen :: [ "Auditorium (40 seats)" :: {}, "Boardroom (12 seats)" :: {}, "Focus pod (4 seats)" :: {} ], unchosen :: {} ] }
blankBooking = { "Meeting title": "", "Room": .unchosen {}, "Duration (min)": .unchosen {}, "Attendees": { current: justTheOrganizer, min: justTheOrganizer, max: justTheOrganizer, step: .discrete 1.0 }, "Include a Teams link": false, "Catering": .none {} }

seatsInRoom :: { "Attendees" :: { current :: Number, max :: Number, min :: Number, step :: [ continuous :: {}, discrete :: Number ] }, "Catering" :: [ none :: {}, ordered :: [ "coffee and pastries" :: {}, "sandwich lunch" :: {} ] ], "Duration (min)" :: [ chosen :: [ "15" :: {}, "30" :: {}, "60" :: {} ], unchosen :: {} ], "Include a Teams link" :: Boolean, "Meeting title" :: String, "Room" :: [ chosen :: [ "Auditorium (40 seats)" :: {}, "Boardroom (12 seats)" :: {}, "Focus pod (4 seats)" :: {} ], unchosen :: {} ] } -> { "Attendees" :: { current :: Number, max :: Number, min :: Number, step :: [ continuous :: {}, discrete :: Number ] }, "Catering" :: [ none :: {}, ordered :: [ "coffee and pastries" :: {}, "sandwich lunch" :: {} ] ], "Duration (min)" :: [ chosen :: [ "15" :: {}, "30" :: {}, "60" :: {} ], unchosen :: {} ], "Include a Teams link" :: Boolean, "Meeting title" :: String, "Room" :: [ chosen :: [ "Auditorium (40 seats)" :: {}, "Boardroom (12 seats)" :: {}, "Focus pod (4 seats)" :: {} ], unchosen :: {} ] }
seatsInRoom booking@{ "Room": room, "Attendees": seats } = match
  { chosen: \r -> booking { "Attendees" = seats { current = seatedIn r seats.current, max = roomCapacity r } }
  , unchosen: \_ -> booking
  } room

seatedIn :: [ "Focus pod (4 seats)" :: {}, "Boardroom (12 seats)" :: {}, "Auditorium (40 seats)" :: {} ] -> Number -> Number
seatedIn room n = clamp justTheOrganizer (roomCapacity room) n

plan :: { "Attendees" :: { current :: Number, max :: Number, min :: Number, step :: [ continuous :: {}, discrete :: Number ] }, "Catering" :: [ none :: {}, ordered :: [ "coffee and pastries" :: {}, "sandwich lunch" :: {} ] ], "Duration (min)" :: [ chosen :: [ "15" :: {}, "30" :: {}, "60" :: {} ], unchosen :: {} ], "Include a Teams link" :: Boolean, "Meeting title" :: String, "Room" :: [ chosen :: [ "Auditorium (40 seats)" :: {}, "Boardroom (12 seats)" :: {}, "Focus pod (4 seats)" :: {} ], unchosen :: {} ] } -> [ complete :: { "Include a Teams link" :: Boolean, "Meeting title" :: String, attendees :: Number, catering :: [ none :: {}, ordered :: [ "coffee and pastries" :: {}, "sandwich lunch" :: {} ] ], duration :: [ "15" :: {}, "30" :: {}, "60" :: {} ], room :: [ "Auditorium (40 seats)" :: {}, "Boardroom (12 seats)" :: {}, "Focus pod (4 seats)" :: {} ] }, incomplete :: {} ]
plan { "Meeting title": title, "Room": room, "Duration (min)": duration, "Attendees": seats, "Include a Teams link": online, "Catering": catering } = match
  { chosen: \r -> match
    { chosen: \d -> .complete { "Meeting title": title, room: r, duration: d, attendees: seats.current, "Include a Teams link": online, catering }
    , unchosen: \_ -> .incomplete {}
    } duration
  , unchosen: \_ -> .incomplete {}
  } room

planLine :: { "Include a Teams link" :: Boolean, "Meeting title" :: String, attendees :: Number, catering :: [ none :: {}, ordered :: [ "coffee and pastries" :: {}, "sandwich lunch" :: {} ] ], duration :: [ "15" :: {}, "30" :: {}, "60" :: {} ], room :: [ "Auditorium (40 seats)" :: {}, "Boardroom (12 seats)" :: {}, "Focus pod (4 seats)" :: {} ] } -> String
planLine p =
  "Plan: " <> titleText p."Meeting title" <> " in the " <> roomText p.room <> ", " <> caseText p.duration <> " min, " <> headcount p.attendees <> " attendees" <> onlineNote p."Include a Teams link" <> cateringNote p.catering

bookedLine :: { "Include a Teams link" :: Boolean, "Meeting title" :: String, attendees :: Number, catering :: [ none :: {}, ordered :: [ "coffee and pastries" :: {}, "sandwich lunch" :: {} ] ], duration :: [ "15" :: {}, "30" :: {}, "60" :: {} ], room :: [ "Auditorium (40 seats)" :: {}, "Boardroom (12 seats)" :: {}, "Focus pod (4 seats)" :: {} ] } -> String
bookedLine p =
  "Booked: " <> titleText p."Meeting title" <> " — " <> roomText p.room <> " for " <> caseText p.duration <> " min"

headcount :: Number -> String
headcount attendees = show (round attendees)

onlineNote :: Boolean -> String
onlineNote online = if online then ", with a Teams link" else ""

cateringNote :: [ ordered :: [ "coffee and pastries" :: {}, "sandwich lunch" :: {} ], none :: {} ] -> String
cateringNote = match { ordered: \c -> ", " <> caseText c, none: \_ -> "" }

titleText :: String -> String
titleText title = case trim title of
  "" -> "Untitled meeting"
  name -> name

roomText :: [ "Focus pod (4 seats)" :: {}, "Boardroom (12 seats)" :: {}, "Auditorium (40 seats)" :: {} ] -> String
roomText = match { "Focus pod (4 seats)": \_ -> "focus pod", "Boardroom (12 seats)": \_ -> "boardroom", "Auditorium (40 seats)": \_ -> "auditorium" }

ratedRoom :: { "Attendees" :: { current :: Number, max :: Number, min :: Number, step :: [ continuous :: {}, discrete :: Number ] }, "Catering" :: [ none :: {}, ordered :: [ "coffee and pastries" :: {}, "sandwich lunch" :: {} ] ], "Duration (min)" :: [ chosen :: [ "15" :: {}, "30" :: {}, "60" :: {} ], unchosen :: {} ], "Include a Teams link" :: Boolean, "Meeting title" :: String, "Room" :: [ chosen :: [ "Auditorium (40 seats)" :: {}, "Boardroom (12 seats)" :: {}, "Focus pod (4 seats)" :: {} ], unchosen :: {} ] } -> [ rated :: { rating :: Number }, unrated :: {} ]
ratedRoom { "Room": room } = match { chosen: \r -> .rated { rating: roomRating r }, unchosen: \_ -> .unrated {} } room

roomRating :: [ "Focus pod (4 seats)" :: {}, "Boardroom (12 seats)" :: {}, "Auditorium (40 seats)" :: {} ] -> Number
roomRating = match { "Focus pod (4 seats)": \_ -> 4.5, "Boardroom (12 seats)": \_ -> 3.5, "Auditorium (40 seats)": \_ -> 4.0 }

seatsTaken :: { "Attendees" :: { current :: Number, max :: Number, min :: Number, step :: [ continuous :: {}, discrete :: Number ] }, "Catering" :: [ none :: {}, ordered :: [ "coffee and pastries" :: {}, "sandwich lunch" :: {} ] ], "Duration (min)" :: [ chosen :: [ "15" :: {}, "30" :: {}, "60" :: {} ], unchosen :: {} ], "Include a Teams link" :: Boolean, "Meeting title" :: String, "Room" :: [ chosen :: [ "Auditorium (40 seats)" :: {}, "Boardroom (12 seats)" :: {}, "Focus pod (4 seats)" :: {} ], unchosen :: {} ] } -> [ seated :: { occupancy :: Number }, unseated :: {} ]
seatsTaken { "Room": room, "Attendees": seats } = match { chosen: \r -> .seated { occupancy: seats.current / roomCapacity r }, unchosen: \_ -> .unseated {} } room

roomCapacity :: [ "Focus pod (4 seats)" :: {}, "Boardroom (12 seats)" :: {}, "Auditorium (40 seats)" :: {} ] -> Number
roomCapacity = match { "Focus pod (4 seats)": \_ -> 4.0, "Boardroom (12 seats)": \_ -> 12.0, "Auditorium (40 seats)": \_ -> 40.0 }

justTheOrganizer :: Number
justTheOrganizer = 1.0

roomStars :: { rating :: Number } -> Number
roomStars = _.rating

seatOccupancy :: { occupancy :: Number } -> Number
seatOccupancy = _.occupancy

roomRatingCaption :: { rating :: Number } -> String
roomRatingCaption _ = "How attendees rated this room"

seatsTakenCaption :: { occupancy :: Number } -> String
seatsTakenCaption _ = "Seats taken"
