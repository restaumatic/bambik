module FlightBookerBootstrap (flightBookerBootstrap) where

import Prelude (Unit, (#), ($))

import Data.Profunctor.Row.VariantToRecord as VariantToRecord
import Effect (Effect)
import FlightBookerViewModel (bookedLine, bookingState, itinerarySettleTime, oneWayLine, plannedTrip, problemLine, rejectedLine, returnLine, submit)
import PUI (action, atCase, blank, debounced, mvu, state)
import PUI.Web ((<+>), choice, inCase, shownWhen, text)
import PUI.Web.Bootstrap (body, button, select, textField, toast)
import PUI.Web.HTML (p)
import QualifiedDo.Semigroupoid as Semigroupoid

flightBookerBootstrap :: Effect Unit
flightBookerBootstrap =
  body $ Semigroupoid.do
    ( Semigroupoid.do
      select @"Flight type" {}
        (choice @"one-way" <+> choice @"return")
      textField @"Start date (DD.MM.YYYY)" {}
      textField @"Return date (DD.MM.YYYY)" {} # inCase @"return" _."Flight type"
    ) # mvu plannedTrip
    ( Semigroupoid.do
      ( Semigroupoid.do
        state @"problem" @String
        p (text problemLine) ) # shownWhen @"problem" bookingState
      ( Semigroupoid.do
        state @"out" @{ y :: Int, m :: Int, d :: Int }
        p (text oneWayLine) ) # shownWhen @"one-way" bookingState
      ( Semigroupoid.do
        state @"out" @{ y :: Int, m :: Int, d :: Int }
        state @"back" @{ y :: Int, m :: Int, d :: Int }
        p (text returnLine) ) # shownWhen @"return" bookingState ) # debounced itinerarySettleTime
    button @"Book" {}
    blank # action @[ booked :: [ oneWayOn :: { y :: Int, m :: Int, d :: Int }, returnBetween :: { out :: { y :: Int, m :: Int, d :: Int }, back :: { y :: Int, m :: Int, d :: Int } } ], rejected :: String ] submit # atCase @"Book"
    VariantToRecord.do
      toast @"booked" bookedLine
      toast @"rejected" rejectedLine
