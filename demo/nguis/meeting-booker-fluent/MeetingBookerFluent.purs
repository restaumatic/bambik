module MeetingBookerFluent (meetingBookerFluent) where

import Prelude (Unit, ($), (#))

import Data.Profunctor.Row.RecordToRecord as RecordToRecord
import Effect (Effect)
import MeetingBookerLogic (blankBooking, bookedLine, chooseSeats, completePlan, durationText, headcount, onlineNote, ratedRoom, roomText, seatsFor, seatsTaken, titleText)
import PUI (completed, displayed, forCase, projection, informed, mvu, optional, projected, tapped, updated)
import PUI.Web.Fluent (body1, button, caption1, card, divider, dropdown, messageBar, progressBar, radioGroup, ratingDisplay, slider, textField, toggleSwitch)
import PUI.Web.HTML (body, div, provided, staticText, text)
import QualifiedDo.Semigroupoid as Semigroupoid

meetingBookerFluent :: Effect Unit
meetingBookerFluent =
  body $
    card { caption: "Book a meeting room" } $ Semigroupoid.do
      ( Semigroupoid.do
          ( RecordToRecord.do
              textField @"Meeting title" {}
              dropdown @"Room" {}
                [ { value: .focusPod {}, label: "Focus pod (4 seats)" }
                , { value: .boardroom {}, label: "Boardroom (12 seats)" }
                , { value: .auditorium {}, label: "Auditorium (40 seats)" }
                ] # optional
              radioGroup @"Duration" {}
                [ { value: .quarter {}, label: "15 min" }
                , { value: .half {}, label: "30 min" }
                , { value: .hour {}, label: "60 min" }
                ] # optional
              toggleSwitch @"Include a Teams link" {}
              divider ) # completed
          (slider @"Attendees" {}) # provided seatsFor # updated (informed chooseSeats)
      ) # mvu blankBooking
      ( div $ RecordToRecord.do
          caption1 $ staticText "How attendees rated this room"
          ratingDisplay @"rating" ) # provided ratedRoom # displayed
      ( div $ RecordToRecord.do
          caption1 $ staticText "Seats taken"
          progressBar @"occupancy" ) # provided seatsTaken # displayed
      ( Semigroupoid.do
          body1 ( RecordToRecord.do
              staticText "Plan: "
              text @"Meeting title" # projection titleText
              staticText " in the "
              text @"Room" # projection roomText
              staticText ", "
              text @"Duration" # projection durationText
              staticText ", "
              text @"attendees" # projection headcount
              staticText " attendees"
              text @"onlineNote" # projected onlineNote ) # tapped
          button @"Book the room" {} ) # provided completePlan
      messageBar # forCase @"Book the room" bookedLine
