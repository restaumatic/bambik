module MeetingBookerFluent (meetingBookerFluent) where

import Prelude (Unit, ($), (#))

import Data.Profunctor.Row.RecordToRecord as RecordToRecord
import Effect (Effect)
import MeetingBookerViewModel (blankBooking, bookedLine, plan, planLine, ratedRoom, roomRatingCaption, roomStars, seatOccupancy, seatsInRoom, seatsTaken, seatsTakenCaption)
import PUI (mvu, settled, state)
import PUI.Web ((<+>), choice, inCase, provided, shown, shownWhen, text)
import PUI.Web.Fluent (body, body1, button, caption1, card, divider, dropdownOptional, dropdownUnpicked, messageBar, progressBar, radioGroupUnpicked, ratingDisplay, slider, textField, toggleSwitch)
import PUI.Web.HTML (div)
import QualifiedDo.Semigroupoid as Semigroupoid

meetingBookerFluent :: Effect Unit
meetingBookerFluent =
  body $ Semigroupoid.do
    ( Semigroupoid.do
      textField @"Meeting title" {}
      dropdownUnpicked @"Room" @"chosen" {}
        (choice @"Focus pod (4 seats)" <+> choice @"Boardroom (12 seats)" <+> choice @"Auditorium (40 seats)") # settled seatsInRoom
      radioGroupUnpicked @"Duration (min)" @"chosen" {}
        (choice @"15" <+> choice @"30" <+> choice @"60")
      dropdownOptional @"Catering" @"ordered" @"none" {}
        (choice @"coffee and pastries" <+> choice @"sandwich lunch")
      toggleSwitch @"Include a Teams link" {}
      divider # shown
      slider @"Attendees" {} # inCase @"chosen" _."Room"
    ) # mvu blankBooking
    ( Semigroupoid.do
      state @"rating" @Number
      div $ RecordToRecord.do
        caption1 $ text roomRatingCaption
        ratingDisplay @"Room rating" roomStars ) # shownWhen @"rated" ratedRoom
    ( Semigroupoid.do
      state @"occupancy" @Number
      div $ RecordToRecord.do
        caption1 $ text seatsTakenCaption
        progressBar @"Seats taken" seatOccupancy ) # shownWhen @"seated" seatsTaken
    ( card $ Semigroupoid.do
      state @"Meeting title" @String
      state @"room" @[ "Focus pod (4 seats)" :: {}, "Boardroom (12 seats)" :: {}, "Auditorium (40 seats)" :: {} ]
      state @"duration" @[ "15" :: {}, "30" :: {}, "60" :: {} ]
      state @"attendees" @Number
      state @"Include a Teams link" @Boolean
      state @"catering" @[ none :: {}, ordered :: [ "coffee and pastries" :: {}, "sandwich lunch" :: {} ] ]
      body1 (text planLine) # shown
      button @"Book the room" {} ) # provided @"complete" plan
    messageBar @"Book the room" bookedLine
