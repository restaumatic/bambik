module MeetingBooker (Booking, Catering, Duration, Headcount, Refreshments, Room, blankBooking, bookRoom, plan, planLine, roomStars, seatOccupancy, seatsInRoom) where

import Prelude (discard, pure, (<>), show, (/), ($))

import Effect.Aff (Aff, Milliseconds(..), delay)
import Data.Int (round)
import Data.Ord (clamp)
import Data.String (toLower, trim)
import Data.Variant (match)
import Data.Variant.Case (caseText)

type Room = [ "Auditorium" :: {}, "Boardroom" :: {}, "Focus pod" :: {} ]

type Duration = [ "15 min" :: {}, "30 min" :: {}, "60 min" :: {} ]

type Refreshments = [ "Coffee and pastries" :: {}, "Sandwich lunch" :: {} ]

type Catering = [ none :: {}, ordered :: Refreshments ]

type Headcount =
  { current :: Number
  , max :: Number
  , min :: Number
  , step :: [ continuous :: {}, discrete :: Number ]
  }

type Booking =
  { "Attendees" :: Headcount
  , "Catering" :: Catering
  , "Duration" :: [ chosen :: Duration, unchosen :: {} ]
  , "Meeting title" :: String
  , "Room" :: [ chosen :: Room, unchosen :: {} ]
  , "Teams link" :: Boolean
  }

type Plan =
  { "Meeting title" :: String
  , room :: Room
  , duration :: Duration
  , attendees :: Number
  , "Teams link" :: Boolean
  , catering :: Catering
  }

blankBooking :: Booking
blankBooking = { "Meeting title": "", "Room": .unchosen {}, "Duration": .unchosen {}, "Attendees": { current: justTheOrganizer, min: justTheOrganizer, max: justTheOrganizer, step: .discrete 1.0 }, "Teams link": false, "Catering": .none {} }

seatsInRoom :: Booking -> Booking
seatsInRoom booking@{ "Room": room, "Attendees": seats } = match
  { chosen: \r -> booking { "Attendees" = seats { current = seatedIn r seats.current, max = roomCapacity r } }
  , unchosen: \_ -> booking
  } room

seatedIn :: Room -> Number -> Number
seatedIn room n = clamp justTheOrganizer (roomCapacity room) n

plan :: Booking -> [ complete :: {}, incomplete :: {} ]
plan booking = match { complete: \_ -> .complete {}, incomplete: \_ -> .incomplete {} } (planned booking)

planned
  :: forall r1.
     { "Meeting title" :: String
     , "Room" :: [ chosen :: Room, unchosen :: {} ]
     , "Duration" :: [ chosen :: Duration, unchosen :: {} ]
     , "Attendees" :: Headcount
     , "Teams link" :: Boolean
     , "Catering" :: Catering
     | r1
     }
  -> [ complete :: Plan, incomplete :: {} ]
planned { "Meeting title": title, "Room": room, "Duration": duration, "Attendees": seats, "Teams link": online, "Catering": catering } = match
  { chosen: \r -> match
    { chosen: \d -> .complete { "Meeting title": title, room: r, duration: d, attendees: seats.current, "Teams link": online, catering }
    , unchosen: \_ -> .incomplete {}
    } duration
  , unchosen: \_ -> .incomplete {}
  } room

planLine :: Booking -> String
planLine booking = match { complete: \p -> "Plan: " <> planText p, incomplete: \_ -> "" } (planned booking)

planText :: Plan -> String
planText p =
  titleText p."Meeting title" <> " in the " <> roomText p.room <> ", " <> caseText p.duration <> ", " <> headcount p.attendees <> " attendees" <> onlineNote p."Teams link" <> cateringNote p.catering

headcount :: Number -> String
headcount attendees = show (round attendees)

onlineNote :: Boolean -> String
onlineNote online = if online then ", with a Teams link" else ""

cateringNote :: Catering -> String
cateringNote = match { ordered: \c -> ", " <> toLower (caseText c), none: \_ -> "" }

titleText :: String -> String
titleText title = case trim title of
  "" -> "Untitled meeting"
  name -> name

roomText :: Room -> String
roomText room = toLower (caseText room)


roomRating :: Room -> Number
roomRating = match { "Focus pod": \_ -> 4.5, "Boardroom": \_ -> 3.5, "Auditorium": \_ -> 4.0 }


roomCapacity :: Room -> Number
roomCapacity = match { "Focus pod": \_ -> 4.0, "Boardroom": \_ -> 12.0, "Auditorium": \_ -> 40.0 }

justTheOrganizer :: Number
justTheOrganizer = 1.0

roomStars :: Booking -> Number
roomStars { "Room": room } = match { chosen: roomRating, unchosen: \_ -> 0.0 } room

seatOccupancy :: Booking -> Number
seatOccupancy { "Room": room, "Attendees": seats } = match { chosen: \r -> seats.current / roomCapacity r, unchosen: \_ -> 0.0 } room

bookRoom :: Booking -> Aff [ "Room booked" :: Booking ]
bookRoom booking = do
  delay (Milliseconds 600.0)
  pure $ ."Room booked" booking
