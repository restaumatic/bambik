module MeetingBooker.Fluent (meetingBooker) where

import Prelude (($), (#), identity, Unit)

import Effect (Effect)
import MeetingBooker (blankBooking, bookRoom, plan, planLine, roomStars, seatOccupancy, seatsInRoom)
import PUI (action, looped, settled, with)
import PUI.Web ((<+>), choice, inCaseAt, provided, staticText, text)
import PUI.Web.Fluent (body, body1, button, caption1, card, divider, dropdownOptional, dropdownUnpicked, indeterminateLinearProgress, messageBar, progressBar, radioGroupUnpicked, ratingDisplay, slider, textField, toggleSwitch)
import PUI.Web.HTML (div)
import QualifiedDo.Semigroupoid as Semigroupoid

meetingBooker :: Effect Unit
meetingBooker =
  body $ Semigroupoid.do
    ( Semigroupoid.do
      textField @"Meeting title"
      dropdownUnpicked @"Room" @"chosen"
        (choice @"Focus pod" <+> choice @"Boardroom" <+> choice @"Auditorium") # settled seatsInRoom
      radioGroupUnpicked @"Duration" @"chosen"
        (choice @"15 min" <+> choice @"30 min" <+> choice @"60 min")
      dropdownOptional @"Catering" @"ordered" @"none"
        (choice @"Coffee and pastries" <+> choice @"Sandwich lunch")
      toggleSwitch @"Teams link"
      divider $ slider @"Attendees" # inCaseAt @"chosen" @"Room"
    ) # looped # with blankBooking
    ( div $ Semigroupoid.do
      caption1 $ staticText "How attendees rated this room"
      ratingDisplay roomStars ) # inCaseAt @"chosen" @"Room"
    ( div $ Semigroupoid.do
      caption1 $ staticText "Seats taken"
      progressBar seatOccupancy ) # inCaseAt @"chosen" @"Room"
    ( card $ Semigroupoid.do
      body1 $ text planLine
      button @"Book the room" ) # provided @"complete" plan
    indeterminateLinearProgress # action @"Book the room" bookRoom
    messageBar @"Room booked" identity
