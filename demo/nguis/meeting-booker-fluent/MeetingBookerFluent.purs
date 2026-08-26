module MeetingBookerFluent (meetingBookerFluent) where

import Prelude (Unit, ($), (#))

import Data.Profunctor.Row.RecordToRecord as RecordToRecord
import Data.Variant.Case (caseText)
import Effect (Effect)
import MeetingBookerLogic (blankBooking, bookedLine, completePlan, headcount, onlineNote, ratedRoom, roomChoice, roomText, seatsInRoom, seatsTaken, titleText)
import PUI (forCase, projection, mvu, optional, projected, settled)
import PUI.Web.Fluent (body1, button, caption1, card, divider, dropdown, messageBar, progressBar, radioGroup, ratingDisplay, slider, textField, toggleSwitch)
import PUI.Web (choice)
import PUI.Web.HTML (inCase, shownWhen, shown, body, div, provided, staticText, text)
import QualifiedDo.Category as Category

meetingBookerFluent :: Effect Unit
meetingBookerFluent =
  body $
    card $ Category.do
      ( Category.do
          textField @"Meeting title" {}
          dropdown @"Room" {}
            [ choice @"Focus pod (4 seats)", choice @"Boardroom (12 seats)", choice @"Auditorium (40 seats)" ] # optional # settled seatsInRoom
          radioGroup @"Duration (min)" {}
            [ choice @"15", choice @"30", choice @"60" ] # optional
          toggleSwitch @"Include a Teams link" {}
          divider # shown
          slider @"Attendees" {} # inCase @"chosen" roomChoice
      ) # mvu blankBooking
      ( div $ RecordToRecord.do
          caption1 $ staticText "How attendees rated this room"
          ratingDisplay @"rating" ) # shownWhen ratedRoom
      ( div $ RecordToRecord.do
          caption1 $ staticText "Seats taken"
          progressBar @"occupancy" ) # shownWhen seatsTaken
      ( Category.do
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
              text @"onlineNote" # projected onlineNote ) # shown
          button @"Book the room" {} ) # provided completePlan
      messageBar # forCase @"Book the room" bookedLine
