module MeetingBookerFluent (meetingBookerFluent) where

import Prelude (Unit, show, ($), (#), (/), (<$>), (<*>), (<>))

import Data.Int (round)
import Data.Maybe (Maybe(..))
import Data.Ord (clamp)
import Data.Profunctor.Row.RecordToRecord as RecordToRecord
import Data.String (trim)
import Data.Variant (match)
import Effect (Effect)
import PUI (PUI, asCase, asField, completed, displayed, forCase, mvu, optional, projected, tapped, updated)
import PUI.Fluent (body1, button, caption1, card, divider, dropdown, messageBar, progressBar, radioGroup, ratingDisplay, slider, textField, toggleSwitch)
import PUI.HTML (body, div, provided, staticText, text)
import PUI.Web (Web)
import QualifiedDo.Semigroupoid as Semigroupoid

meetingBookerFluent :: Effect Unit
meetingBookerFluent =
  body $
    card { caption: "Book a meeting room" } Semigroupoid.do
      ( Semigroupoid.do
          ( RecordToRecord.do
              textField { label: "Meeting title" } # asField @"title"
              dropdown { label: "Room" }
                [ { value: .focusPod {}, label: "Focus pod (4 seats)" }
                , { value: .boardroom {}, label: "Boardroom (12 seats)" }
                , { value: .auditorium {}, label: "Auditorium (40 seats)" }
                ] # optional # asField @"room"
              radioGroup { label: "Duration" }
                [ { value: .quarter {}, label: "15 min" }
                , { value: .half {}, label: "30 min" }
                , { value: .hour {}, label: "60 min" }
                ] # optional # asField @"duration"
              toggleSwitch { label: "Include a Teams link" } # asField @"online"
              divider ) # completed
          (slider { label: "Attendees" } # asField @"attendees") # provided seatsFor # updated chooseSeats
      ) # mvu blankBooking
      ( div $ RecordToRecord.do
          caption1 $ staticText "How attendees rated this room"
          ratingDisplay ) # provided ratedRoom # displayed
      ( div $ RecordToRecord.do
          caption1 $ staticText "Seats taken"
          progressBar ) # provided seatsTaken # displayed
      ( Semigroupoid.do
          body1 ( RecordToRecord.do
              staticText "Plan: "
              text # projected planLine ) # tapped
          button { label: "Book the room" } # asCase @"booked"
      ) # provided completePlan
      bookedBar

blankBooking :: { title :: String, room :: Maybe [ focusPod :: {}, boardroom :: {}, auditorium :: {} ], duration :: Maybe [ quarter :: {}, half :: {}, hour :: {} ], attendees :: Number, online :: Boolean }
blankBooking = { title: "", room: Nothing, duration: Nothing, attendees: justTheOrganizer, online: false }

seatsFor :: { room :: Maybe [ focusPod :: {}, boardroom :: {}, auditorium :: {} ], attendees :: Number } -> Maybe { attendees :: { current :: Number, min :: Number, max :: Number, step :: Maybe Number } }
seatsFor { room, attendees } = (\r -> { attendees: { current: seatedIn r attendees, min: justTheOrganizer, max: roomCapacity r, step: Just 1.0 } }) <$> room

chooseSeats :: { attendees :: { current :: Number, min :: Number, max :: Number, step :: Maybe Number } } -> { attendees :: Number } -> { attendees :: Number }
chooseSeats { attendees } m = m { attendees = attendees.current }

seatedIn :: [ focusPod :: {}, boardroom :: {}, auditorium :: {} ] -> Number -> Number
seatedIn room n = clamp justTheOrganizer (roomCapacity room) n

completePlan :: { title :: String, room :: Maybe [ focusPod :: {}, boardroom :: {}, auditorium :: {} ], duration :: Maybe [ quarter :: {}, half :: {}, hour :: {} ], attendees :: Number, online :: Boolean } -> Maybe { title :: String, room :: [ focusPod :: {}, boardroom :: {}, auditorium :: {} ], duration :: [ quarter :: {}, half :: {}, hour :: {} ], attendees :: Number, online :: Boolean }
completePlan { title, room, duration, attendees, online } =
  (\r d -> { title, room: r, duration: d, attendees: seatedIn r attendees, online }) <$> room <*> duration

bookedBar :: PUI Web [ booked :: { title :: String, room :: [ focusPod :: {}, boardroom :: {}, auditorium :: {} ], duration :: [ quarter :: {}, half :: {}, hour :: {} ], attendees :: Number, online :: Boolean } ] {}
bookedBar = messageBar # forCase @"booked" bookedLine

bookedLine :: { title :: String, room :: [ focusPod :: {}, boardroom :: {}, auditorium :: {} ], duration :: [ quarter :: {}, half :: {}, hour :: {} ], attendees :: Number, online :: Boolean } -> String
bookedLine { title, room, duration } =
  "Booked: " <> titleText { title } <> " — " <> roomText room <> " for " <> durationText duration

planLine :: { title :: String, room :: [ focusPod :: {}, boardroom :: {}, auditorium :: {} ], duration :: [ quarter :: {}, half :: {}, hour :: {} ], attendees :: Number, online :: Boolean } -> String
planLine { title, room, duration, attendees, online } =
  titleText { title }
    <> " in the " <> roomText room
    <> ", " <> durationText duration
    <> ", " <> show (round attendees) <> " attendees"
    <> (if online then ", with a Teams link" else "")

titleText :: { title :: String } -> String
titleText { title } = case trim title of
  "" -> "Untitled meeting"
  name -> name

roomText :: [ focusPod :: {}, boardroom :: {}, auditorium :: {} ] -> String
roomText = match { focusPod: \_ -> "focus pod", boardroom: \_ -> "boardroom", auditorium: \_ -> "auditorium" }

durationText :: [ quarter :: {}, half :: {}, hour :: {} ] -> String
durationText = match { quarter: \_ -> "15 min", half: \_ -> "30 min", hour: \_ -> "60 min" }

ratedRoom :: { room :: Maybe [ focusPod :: {}, boardroom :: {}, auditorium :: {} ] } -> Maybe { value :: Number }
ratedRoom { room } = (\r -> { value: roomRating r }) <$> room

roomRating :: [ focusPod :: {}, boardroom :: {}, auditorium :: {} ] -> Number
roomRating = match { focusPod: \_ -> 4.5, boardroom: \_ -> 3.5, auditorium: \_ -> 4.0 }

seatsTaken :: { room :: Maybe [ focusPod :: {}, boardroom :: {}, auditorium :: {} ], attendees :: Number } -> Maybe { value :: Number }
seatsTaken { room, attendees } = (\r -> { value: seatedIn r attendees / roomCapacity r }) <$> room

roomCapacity :: [ focusPod :: {}, boardroom :: {}, auditorium :: {} ] -> Number
roomCapacity = match { focusPod: \_ -> 4.0, boardroom: \_ -> 12.0, auditorium: \_ -> 40.0 }

justTheOrganizer :: Number
justTheOrganizer = 1.0
