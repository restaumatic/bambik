module MeetingBookerFluent (meetingBookerFluent) where

import Prelude (Unit, ($), (#))

import Data.Profunctor.Row.RecordToRecord as RecordToRecord
import Data.Variant (match)
import Effect (Effect)
import MeetingBookerLogic (blankBooking, bookedLine, plan, ratedRoom, roomOf, seatsInRoom, seatsTaken)
import PUI (forCases, mvu, optional, settled)
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
            [ choice @"Focus pod (4 seats)", choice @"Boardroom (12 seats)", choice @"Auditorium (40 seats)" ] # optional @"chosen" @"unchosen" # settled seatsInRoom
          radioGroup @"Duration (min)" {}
            [ choice @"15", choice @"30", choice @"60" ] # optional @"chosen" @"unchosen"
          toggleSwitch @"Include a Teams link" {}
          divider # shown
          slider @"Attendees" {} # inCase @"chosen" roomOf
      ) # mvu blankBooking
      ( div $ RecordToRecord.do
          caption1 $ staticText "How attendees rated this room"
          ratingDisplay @"rating" ) # shownWhen @"rated" ratedRoom
      ( div $ RecordToRecord.do
          caption1 $ staticText "Seats taken"
          progressBar @"occupancy" ) # shownWhen @"seated" seatsTaken
      ( Category.do
          ( body1 $ RecordToRecord.do
              staticText "Plan: "
              text @"titleText"
              staticText " in the "
              text @"roomText"
              staticText ", "
              text @"durationText"
              staticText " min, "
              text @"attendeesText"
              staticText " attendees"
              text @"onlineNote" ) # shown
          button @"Book the room" {} ) # provided @"complete" plan
      messageBar # forCases (match { "Book the room": bookedLine })
