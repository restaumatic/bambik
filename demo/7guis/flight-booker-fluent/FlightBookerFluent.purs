module FlightBookerFluent (flightBookerFluent) where

import Prelude (Unit, (#), ($))

import Data.Profunctor.Row.VariantToRecord as VariantToRecord
import Effect (Effect)
import FlightBookerViewModel (bookedLine, bookingState, itinerarySettleTime, oneWayLine, plannedTrip, problemLine, rejectedLine, returnLine, submit)
import PUI (action, atCase, debounced, mvu, blank)
import PUI.Web ((<+>), choice, inCase, shownWhen, text)
import PUI.Web.Fluent (body, body1, button, dropdown, messageBar, textField)
import QualifiedDo.Semigroupoid as Semigroupoid

flightBookerFluent :: Effect Unit
flightBookerFluent =
  body $ Semigroupoid.do
    ( Semigroupoid.do
      dropdown @"Flight type" {}
        (choice @"one-way" <+> choice @"return")
      textField @"Start date (DD.MM.YYYY)" {}
      textField @"Return date (DD.MM.YYYY)" {} # inCase @"return" _."Flight type"
    ) # mvu
      @( "Flight type" :: [ "one-way" :: {}, "return" :: {} ]
       , "Start date (DD.MM.YYYY)" :: String
       , "Return date (DD.MM.YYYY)" :: String
       )
      plannedTrip
    ( Semigroupoid.do
      body1 (text problemLine) # shownWhen @"problem" @( problem :: { problem :: String }, "one-way" :: { out :: { y :: Int, m :: Int, d :: Int } }, "return" :: { out :: { y :: Int, m :: Int, d :: Int }, back :: { y :: Int, m :: Int, d :: Int } } ) bookingState
      body1 (text oneWayLine) # shownWhen @"one-way" bookingState
      body1 (text returnLine) # shownWhen @"return" bookingState ) # debounced itinerarySettleTime
    button @"Book" {}
    blank # action @[ booked :: [ oneWayOn :: { y :: Int, m :: Int, d :: Int }, returnBetween :: { out :: { y :: Int, m :: Int, d :: Int }, back :: { y :: Int, m :: Int, d :: Int } } ], rejected :: String ] submit # atCase @"Book"
    VariantToRecord.do
      messageBar @"booked" bookedLine
      messageBar @"rejected" rejectedLine
