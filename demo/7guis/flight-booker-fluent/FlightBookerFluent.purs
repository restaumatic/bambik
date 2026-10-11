module FlightBookerFluent (flightBookerFluent) where

import Prelude (Unit, identity, (#), ($))

import Data.Profunctor.Row.VariantToRecord as VariantToRecord
import Effect (Effect)
import FlightBookerViewModel (bookedLine, bookingState, itinerarySettleTime, oneWayLine, plannedTrip, problemLine, rejectedLine, returnLine, submit)
import PUI (action, atCase, debounced, looped, prefixed, with)
import PUI.Pipeline as Pipeline
import PUI.Web ((<+>), choice, inCaseAt, pane, text)
import PUI.Web.Fluent (body, body1, button, dropdown, messageBar, textField)

flightBookerFluent :: Effect Unit
flightBookerFluent =
  body $ Pipeline.do
    ( Pipeline.do
      dropdown @"Flight type" {}
        (choice @"one-way" <+> choice @"return")
      textField @"Start date (DD.MM.YYYY)" {}
      textField @"Return date (DD.MM.YYYY)" {} # inCaseAt @"return" @"Flight type"
    ) # looped
      @( "Flight type" :: [ "one-way" :: {}, "return" :: {} ]
       , "Start date (DD.MM.YYYY)" :: String
       , "Return date (DD.MM.YYYY)" :: String
       ) # with plannedTrip
    prefixed ( Pipeline.do
      body1 (text problemLine) # pane @"problem"
        @( problem :: { problem :: [ returnBeforeStart :: {}, unreadableReturn :: { input :: String }, unreadableStart :: { input :: String } ] }
         , "one-way" :: { out :: { y :: Int, m :: Int, d :: Int } }
         , "return" :: { out :: { y :: Int, m :: Int, d :: Int }, back :: { y :: Int, m :: Int, d :: Int } }
         ) bookingState
      body1 (text oneWayLine) # pane @"one-way" bookingState
      body1 (text returnLine) # pane @"return" bookingState ) identity # debounced itinerarySettleTime
    button @"Book" {}
    ( VariantToRecord.do
      messageBar @"Flight booked" bookedLine
      messageBar @"Booking rejected" rejectedLine ) # action
        @( "Flight booked" :: [ oneWayOn :: { y :: Int, m :: Int, d :: Int }, returnBetween :: { out :: { y :: Int, m :: Int, d :: Int }, back :: { y :: Int, m :: Int, d :: Int } } ]
         , "Booking rejected" :: [ returnBeforeStart :: {}, unreadableReturn :: { input :: String }, unreadableStart :: { input :: String } ]
         ) submit # atCase @"Book"
