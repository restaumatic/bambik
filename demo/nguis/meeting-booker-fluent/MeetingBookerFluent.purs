module MeetingBookerFluent (meetingBookerFluent) where

import Prelude (identity, Unit, ($), (#))

import Data.Profunctor.Row.RecordToRecord as RecordToRecord
import Data.Variant.Case (caseText)
import Effect (Effect)
import MeetingBookerLogic (blankBooking, bookedLine, chooseSeats, completePlan, headcount, onlineNote, ratedRoom, roomText, seatsFor, seatsTaken, titleText)
import PUI (forCase, projection, informed, mvu, optional, projected, updated)
import PUI.Web.Fluent (body1, button, caption1, card, divider, dropdown, messageBar, progressBar, radioGroup, ratingDisplay, slider, textField, toggleSwitch)
import PUI.Web (choice)
import PUI.Web.HTML (shownWhen, shownAs, body, div, provided, staticText, text)
import QualifiedDo.Semigroupoid as Semigroupoid

meetingBookerFluent :: Effect Unit
meetingBookerFluent =
  body $
    card $ Semigroupoid.do
      ( Semigroupoid.do
          textField @"Meeting title" {}
          dropdown @"Room" {}
            [ choice @"Focus pod (4 seats)", choice @"Boardroom (12 seats)", choice @"Auditorium (40 seats)" ] # optional
          radioGroup @"Duration (min)" {}
            [ choice @"15", choice @"30", choice @"60" ] # optional
          toggleSwitch @"Include a Teams link" {}
          divider # shownAs identity
          (slider @"Attendees" {}) # provided seatsFor # updated (informed chooseSeats)
      ) # mvu blankBooking
      ( div $ RecordToRecord.do
          caption1 $ staticText "How attendees rated this room"
          ratingDisplay @"rating" ) # shownWhen ratedRoom
      ( div $ RecordToRecord.do
          caption1 $ staticText "Seats taken"
          progressBar @"occupancy" ) # shownWhen seatsTaken
      ( Semigroupoid.do
          ( body1 $ RecordToRecord.do
              staticText "Plan: "
              text @"Meeting title" # projection titleText
              staticText " in the "
              text @"Room" # projection roomText
              staticText ", "
              text @"Duration (min)" # projection caseText
              staticText " min, "
              text @"attendees" # projection headcount
              staticText " attendees"
              text @"onlineNote" # projected onlineNote ) # shownAs identity
          button @"Book the room" {} ) # provided completePlan
      messageBar # forCase @"Book the room" bookedLine
