module FlightBookerHTML (flightBookerHTML) where

import Prelude ((#), ($), Unit)

import Data.Profunctor.Row.VariantToRecord as VariantToRecord
import Effect (Effect)
import FlightBookerViewModel (bookedLine, bookingState, itinerarySettleTime, oneWayLine, plannedTrip, problemLine, rejectedLine, returnLine, submit)
import PUI (action, atCase, debounced, looped, with)
import PUI.Web ((<+>), choice, inCase, shown, shownWhen, staticText, text)
import PUI.Web.HTML (body, button, div, input, label, output, p, select)
import QualifiedDo.Semigroupoid as Semigroupoid

flightBookerHTML :: Effect Unit
flightBookerHTML =
  body $ div $ Semigroupoid.do
    ( Semigroupoid.do
      p ( label $ Semigroupoid.do
        (staticText "Flight type ") # shown
        select @"Flight type" (choice @"one-way" <+> choice @"return") )
      p ( Semigroupoid.do
        label $ Semigroupoid.do
          (staticText "Start date ") # shown
          input @"Start date" "text"
        (staticText " DD.MM.YYYY") # shown )
      p ( Semigroupoid.do
        label $ Semigroupoid.do
          (staticText "Return date ") # shown
          input @"Return date" "text"
        (staticText " DD.MM.YYYY") # shown ) # inCase @"return" _."Flight type"
    ) # looped
      @( "Flight type" :: [ "one-way" :: {}, "return" :: {} ]
       , "Start date" :: String
       , "Return date" :: String
       ) # with plannedTrip
    ( Semigroupoid.do
      p (text problemLine) # shownWhen @"problem"
        @( problem :: { problem :: String }
         , "one-way" :: { out :: { y :: Int, m :: Int, d :: Int } }
         , "return" :: { out :: { y :: Int, m :: Int, d :: Int }, back :: { y :: Int, m :: Int, d :: Int } }
         ) bookingState
      p (text oneWayLine) # shownWhen @"one-way" bookingState
      p (text returnLine) # shownWhen @"return" bookingState ) # debounced itinerarySettleTime
    button @"Book" {}
    ( VariantToRecord.do
      output @"Flight booked" bookedLine
      output @"Booking rejected" rejectedLine ) # action
        @( "Flight booked" :: [ oneWayOn :: { y :: Int, m :: Int, d :: Int }, returnBetween :: { out :: { y :: Int, m :: Int, d :: Int }, back :: { y :: Int, m :: Int, d :: Int } } ]
         , "Booking rejected" :: String
         ) submit # atCase @"Book"
