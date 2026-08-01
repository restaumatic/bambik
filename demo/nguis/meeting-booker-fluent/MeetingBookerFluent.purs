module MeetingBookerFluent (meetingBookerFluent) where

import Prelude (Unit, min, show, ($), (#), (/), (<>))

import Data.Int (round)
import Data.Profunctor (lcmap)
import Data.Profunctor.Row.RecordToRecord as RecordToRecord
import Data.String (trim)
import Data.Variant (match)
import Effect (Effect)
import PUI (PUI, asCase, asField, forCase, mvu, projection, required, tapped)
import PUI.Fluent (body1, button, caption1, card, divider, dropdown, messageBar, progressBar, radioGroup, ratingDisplay, slider, textField, toggleSwitch)
import PUI.HTML (body, div, staticText, text)
import PUI.Web (Web)
import QualifiedDo.Semigroupoid as Semigroupoid

meetingBookerFluent :: Effect Unit
meetingBookerFluent =
  body $
    card { caption: "Book a meeting room" } Semigroupoid.do
      ( RecordToRecord.do
          textField { label: "Meeting title" } # asField @"title"
          dropdown { label: "Room" }
            [ { value: .focusPod {}, label: "Focus pod (4 seats)" }
            , { value: .boardroom {}, label: "Boardroom (12 seats)" }
            , { value: .auditorium {}, label: "Auditorium (40 seats)" }
            ] # required # asField @"room"
          radioGroup { label: "Duration" }
            [ { value: .quarter {}, label: "15 min" }
            , { value: .half {}, label: "30 min" }
            , { value: .hour {}, label: "60 min" }
            ] # required # asField @"duration"
          slider { label: "Attendees", min: fewestAttendees, max: largestRoomCapacity, step: attendeeStep } # asField @"attendees"
          toggleSwitch { label: "Include a Teams link" } # asField @"online"
          divider
      ) # mvu weeklySync
      ( div $ RecordToRecord.do
          caption1 $ staticText "How attendees rated this room"
          ratingDisplay ) # projection roomRating # tapped
      ( div $ RecordToRecord.do
          caption1 $ staticText "Seats taken"
          progressBar ) # projection seatsTaken # tapped
      body1 ( RecordToRecord.do
          staticText "Plan: "
          text # projection planLine ) # tapped
      button { label: "Book the room" } # asCase @"booked"
      bookedBar

weeklySync :: { title :: String, room :: [ focusPod :: {}, boardroom :: {}, auditorium :: {} ], duration :: [ quarter :: {}, half :: {}, hour :: {} ], attendees :: Number, online :: Boolean }
weeklySync =
  { title: "Weekly sync"
  , room: .boardroom {}
  , duration: .half {}
  , attendees: 6.0
  , online: true
  }

bookedBar :: PUI Web [ booked :: { title :: String, room :: [ focusPod :: {}, boardroom :: {}, auditorium :: {} ], duration :: [ quarter :: {}, half :: {}, hour :: {} ], attendees :: Number, online :: Boolean } ] {}
bookedBar = messageBar # forCase @"booked" # lcmap (match { booked: \plan -> .booked (bookedLine plan) })

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

roomRating :: { room :: [ focusPod :: {}, boardroom :: {}, auditorium :: {} ] } -> Number
roomRating { room } = match { focusPod: \_ -> 4.5, boardroom: \_ -> 3.5, auditorium: \_ -> 4.0 } room

seatsTaken :: { room :: [ focusPod :: {}, boardroom :: {}, auditorium :: {} ], attendees :: Number } -> Number
seatsTaken { room, attendees } = min 1.0 (attendees / roomCapacity room)

roomCapacity :: [ focusPod :: {}, boardroom :: {}, auditorium :: {} ] -> Number
roomCapacity = match { focusPod: \_ -> 4.0, boardroom: \_ -> 12.0, auditorium: \_ -> 40.0 }

fewestAttendees :: Number
fewestAttendees = 1.0

largestRoomCapacity :: Number
largestRoomCapacity = roomCapacity (.auditorium {})

attendeeStep :: Number
attendeeStep = 1.0
