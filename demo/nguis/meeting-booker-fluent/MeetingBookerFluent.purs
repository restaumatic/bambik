module MeetingBookerFluent (meetingBookerFluent) where

import Prelude (Unit, ($), (#))

import Data.Profunctor.Row.RecordToRecord as RecordToRecord
import Data.Variant.Case (caseText)
import Effect (Effect)
import MeetingBookerLogic (blankBooking, bookedLine, chooseSeats, completePlan, headcount, onlineNote, ratedRoom, roomText, seatsFor, seatsTaken, titleText)
import PUI (completed, displayed, forCase, projection, informed, mvu, optional, projected, tapped, updated)
import PUI.Web.Fluent (body1, button, caption1, card, divider, dropdown, messageBar, progressBar, radioGroup, ratingDisplay, slider, textField, toggleSwitch)
import PUI.Web (choice)
import PUI.Web.HTML (body, div, provided, staticText, text)
import Data.Tuple.Nested ((/\))
import QualifiedDo.Semigroupoid as Semigroupoid

meetingBookerFluent :: Effect Unit
meetingBookerFluent =
  body $
    card { caption: "Book a meeting room" } $ Semigroupoid.do
      ( Semigroupoid.do
          ( RecordToRecord.do
              textField @"Meeting title" {}
              dropdown @"Room" {}
                [ choice @"Focus pod (4 seats)", choice @"Boardroom (12 seats)", choice @"Auditorium (40 seats)" ] # optional
              radioGroup @"Duration" {}
                [ choice @"15 min", choice @"30 min", choice @"60 min" ] # optional
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
              text @"Duration" # projection caseText
              staticText ", "
              text @"attendees" # projection headcount
              staticText " attendees"
              text @"onlineNote" # projected onlineNote ) # tapped
          button @"Book the room" {} ) # provided completePlan
      messageBar # forCase @"Book the room" bookedLine
