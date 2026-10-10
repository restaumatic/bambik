module MeetingBookerFluent (meetingBookerFluent) where

import Prelude (Unit, ($), (#))

import Data.Profunctor.Row.RecordToRecord as RecordToRecord
import Effect (Effect)
import MeetingBookerViewModel (blankBooking, bookedLine, plan, planLine, ratedRoom, roomRatingCaption, roomStars, seatOccupancy, seatsInRoom, seatsTaken, seatsTakenCaption)
import PUI (looped, settled, with)
import PUI.Web ((<+>), choice, inCaseAt, provided, shown, shownWhen, text)
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
      slider @"Attendees" {} # inCaseAt @"chosen" @"Room"
    ) # looped
      @( "Meeting title" :: String
       , "Room" :: [ chosen :: [ "Focus pod (4 seats)" :: {}, "Boardroom (12 seats)" :: {}, "Auditorium (40 seats)" :: {} ], unchosen :: {} ]
       , "Duration (min)" :: [ chosen :: [ "15" :: {}, "30" :: {}, "60" :: {} ], unchosen :: {} ]
       , "Attendees" :: { current :: Number, min :: Number, max :: Number, step :: [ discrete :: Number, continuous :: {} ] }
       , "Include a Teams link" :: Boolean
       , "Catering" :: [ ordered :: [ "coffee and pastries" :: {}, "sandwich lunch" :: {} ], none :: {} ]
       ) # with blankBooking
    ( div $ RecordToRecord.do
      caption1 $ text roomRatingCaption
      ratingDisplay roomStars ) # shownWhen @"rated" @( rated :: { rating :: Number }, unrated :: {} ) ratedRoom
    ( div $ RecordToRecord.do
      caption1 $ text seatsTakenCaption
      progressBar seatOccupancy ) # shownWhen @"seated" @( seated :: { occupancy :: Number }, unseated :: {} ) seatsTaken
    ( card $ Semigroupoid.do
      body1 (text planLine) # shown
      button @"Book the room" {} ) # provided @"complete"
        @( complete :: { "Meeting title" :: String, room :: [ "Focus pod (4 seats)" :: {}, "Boardroom (12 seats)" :: {}, "Auditorium (40 seats)" :: {} ], duration :: [ "15" :: {}, "30" :: {}, "60" :: {} ], attendees :: Number, "Include a Teams link" :: Boolean, catering :: [ ordered :: [ "coffee and pastries" :: {}, "sandwich lunch" :: {} ], none :: {} ] }
         , incomplete :: {}
         ) plan
    messageBar @"Book the room" bookedLine
