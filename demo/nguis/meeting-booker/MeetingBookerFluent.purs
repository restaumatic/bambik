module MeetingBookerFluent (meetingBookerFluent) where

import Prelude (Unit, ($), (#))

import Effect (Effect)
import MeetingBookerViewModel (blankBooking, bookRoom, bookedLine, plan, planLine, ratedRoom, roomStars, seatOccupancy, seatsInRoom, seatsTaken)
import PUI (action, atCase, looped, settled, with)
import PUI.Web ((<+>), choice, inCase, inCaseAt, provided, staticText, text)
import PUI.Web.Fluent (body, body1, button, caption1, card, divider, dropdownOptional, dropdownUnpicked, indeterminateLinearProgress, messageBar, progressBar, radioGroupUnpicked, ratingDisplay, slider, textField, toggleSwitch)
import PUI.Web.HTML (div)
import QualifiedDo.Semigroupoid as Semigroupoid

meetingBookerFluent :: Effect Unit
meetingBookerFluent =
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
    ) # looped
      @( "Meeting title" :: String
       , "Room" :: [ chosen :: [ "Focus pod (4 seats)" :: {}, "Boardroom (12 seats)" :: {}, "Auditorium (40 seats)" :: {} ], unchosen :: {} ]
       , "Duration (min)" :: [ chosen :: [ "15" :: {}, "30" :: {}, "60" :: {} ], unchosen :: {} ]
       , "Attendees" :: { current :: Number, min :: Number, max :: Number, step :: [ discrete :: Number, continuous :: {} ] }
       , "Include a Teams link" :: Boolean
       , "Catering" :: [ ordered :: [ "coffee and pastries" :: {}, "sandwich lunch" :: {} ], none :: {} ]
       ) # with blankBooking
    ( div $ Semigroupoid.do
      caption1 $ staticText "How attendees rated this room"
      ratingDisplay roomStars ) # inCase @"rated" @( rated :: {}, unrated :: {} ) ratedRoom
    ( div $ Semigroupoid.do
      caption1 $ staticText "Seats taken"
      progressBar seatOccupancy ) # inCase @"seated" @( seated :: {}, unseated :: {} ) seatsTaken
    ( card $ Semigroupoid.do
      body1 (text planLine)
      button @"Book the room" ) # provided @"complete" @( complete :: {}, incomplete :: {} ) plan
    ( indeterminateLinearProgress # action
      @( "Room booked" :: { "Meeting title" :: String, "Room" :: [ chosen :: [ "Focus pod (4 seats)" :: {}, "Boardroom (12 seats)" :: {}, "Auditorium (40 seats)" :: {} ], unchosen :: {} ], "Duration (min)" :: [ chosen :: [ "15" :: {}, "30" :: {}, "60" :: {} ], unchosen :: {} ], "Attendees" :: { current :: Number, min :: Number, max :: Number, step :: [ discrete :: Number, continuous :: {} ] }, "Include a Teams link" :: Boolean, "Catering" :: [ ordered :: [ "coffee and pastries" :: {}, "sandwich lunch" :: {} ], none :: {} ] } )
      bookRoom ) # atCase @"Book the room"
    messageBar @"Room booked" bookedLine
