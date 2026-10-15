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
        (choice @"Focus pod (4 seats)" <+> choice @"Boardroom (12 seats)" <+> choice @"Auditorium (40 seats)") # settled seatsInRoom
      radioGroupUnpicked @"Duration (min)" @"chosen"
        (choice @"15" <+> choice @"30" <+> choice @"60")
      dropdownOptional @"Catering" @"ordered" @"none"
        (choice @"coffee and pastries" <+> choice @"sandwich lunch")
      toggleSwitch @"Include a Teams link"
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
    indeterminateLinearProgress # action @"Book the room" @( "Room booked" :: _ ) bookRoom
    messageBar @"Room booked" identity
