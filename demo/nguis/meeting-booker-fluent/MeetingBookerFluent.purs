module MeetingBookerFluent (meetingBookerFluent) where

import Prelude (Unit, ($), (#))

import Data.Profunctor.Row.RecordToRecord as RecordToRecord
import Effect (Effect)
import MeetingBookerLogic (blankBooking, bookedLine, chooseSeats, completePlan, durationText, headcount, onlineNote, ratedRoom, roomText, seatsFor, seatsTaken, titleText)
import PUI (asCase, asField, completed, displayed, forCase, forField, informed, mvu, optional, projected, tapped, updated)
import PUI.Web.Fluent (body1, button, caption1, card, divider, dropdown, messageBar, progressBar, radioGroup, ratingDisplay, slider, textField, toggleSwitch)
import PUI.Web.HTML (body, div, provided, staticText, text)
import QualifiedDo.Semigroupoid as Semigroupoid

meetingBookerFluent :: Effect Unit
meetingBookerFluent =
  body $
    card { caption: "Book a meeting room" } $ Semigroupoid.do
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
          (slider { label: "Attendees" } # asField @"seats") # provided seatsFor # updated (informed chooseSeats)
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
              text # forField @"title" titleText
              staticText " in the "
              text # forField @"room" roomText
              staticText ", "
              text # forField @"duration" durationText
              staticText ", "
              text # forField @"attendees" headcount
              staticText " attendees"
              text # projected onlineNote ) # tapped
          button { label: "Book the room" } # asCase @"booked" ) # provided completePlan
      messageBar # forCase @"booked" bookedLine
