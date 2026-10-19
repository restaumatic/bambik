module MeetingBooker.Fluent (meetingBooker) where

import Prelude (($), (#), identity, Unit)

import Data.Profunctor.Row.VariantToVariant as VariantToVariant
import Effect (Effect)
import MeetingBooker (blankBooking, bookRoom, plan, planLine, roomStars, seatOccupancy, seatsInRoom)
import PUI (action, looped, settled, with)
import PUI.Web (inCaseAt, provided, staticText, text)
import PUI.Web.Fluent (body, body1, button, caption1, card, divider, dropdownOptional, dropdownUnpicked, indeterminateLinearProgress, messageBar, option, progressBar, radio, radioGroupUnpicked, ratingDisplay, slider, textField, toggleSwitch)
import PUI.Web.HTML (div)
import QualifiedDo.Semigroupoid as Semigroupoid

meetingBooker :: Effect Unit
meetingBooker =
  body $ Semigroupoid.do
    ( Semigroupoid.do
      textField @"Meeting title"
      dropdownUnpicked @"Room" @"chosen"
        ( VariantToVariant.do
          option @"Focus pod"
          option @"Boardroom"
          option @"Auditorium"
        ) # settled seatsInRoom
      radioGroupUnpicked @"Duration" @"chosen" $ VariantToVariant.do
        radio @"15 min"
        radio @"30 min"
        radio @"60 min"
      dropdownOptional @"Catering" @"ordered" @"none" $ VariantToVariant.do
        option @"Coffee and pastries"
        option @"Sandwich lunch"
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
