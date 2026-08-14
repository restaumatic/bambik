module MeetingBookerFluent (meetingBookerFluent) where

import Prelude (Unit, ($), (#))

import Data.Profunctor.Row.RecordToRecord as RecordToRecord
import Effect (Effect)
import MeetingBookerLogic (blankBooking, bookedLine, chooseSeats, completePlan, durationText, headcount, onlineNote, ratedRoom, roomText, seatsFor, seatsTaken, titleText)
import PUI (asCase, completed, displayed, forCase, forField, informed, mvu, optional, projected, tapped, updated)
import PUI.Web.Fluent (body1, button, caption1, card, divider, dropdown, messageBar, progressBar, radioGroup, ratingDisplay, slider, textField, toggleSwitch)
import PUI.Web.HTML (body, div, provided, staticText, text)
import QualifiedDo.Semigroupoid as Semigroupoid

meetingBookerFluent :: Effect Unit
meetingBookerFluent =
  body $
    card { caption: "Book a meeting room" } $ Semigroupoid.do
      ( Semigroupoid.do
          ( RecordToRecord.do
              textField @"title" { label: "Meeting title" }
              dropdown @"room" {}
                [ { value: .focusPod {}, label: "Focus pod (4 seats)" }
                , { value: .boardroom {}, label: "Boardroom (12 seats)" }
                , { value: .auditorium {}, label: "Auditorium (40 seats)" }
                ] # optional
              radioGroup @"duration" {}
                [ { value: .quarter {}, label: "15 min" }
                , { value: .half {}, label: "30 min" }
                , { value: .hour {}, label: "60 min" }
                ] # optional
              toggleSwitch @"online" { label: "Include a Teams link" }
              divider ) # completed
          (slider @"seats" { label: "Attendees" }) # provided seatsFor # updated (informed chooseSeats)
      ) # mvu blankBooking
      ( div $ RecordToRecord.do
          caption1 $ staticText "How attendees rated this room"
          ratingDisplay @"value" ) # provided ratedRoom # displayed
      ( div $ RecordToRecord.do
          caption1 $ staticText "Seats taken"
          progressBar @"value" ) # provided seatsTaken # displayed
      ( Semigroupoid.do
          body1 ( RecordToRecord.do
              staticText "Plan: "
              text @"value" # forField @"title" titleText
              staticText " in the "
              text @"value" # forField @"room" roomText
              staticText ", "
              text @"value" # forField @"duration" durationText
              staticText ", "
              text @"value" # forField @"attendees" headcount
              staticText " attendees"
              text @"value" # projected @"value" onlineNote ) # tapped
          button { label: "Book the room" } # asCase @"clicked" @"booked" ) # provided completePlan
      messageBar # forCase @"event" @"booked" bookedLine
