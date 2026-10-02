module FlightBookerHTML (flightBookerHTML) where

import Prelude ((#), ($), Unit)

import Data.Profunctor.Row.VariantToRecord as VariantToRecord
import Effect (Effect)
import FlightBookerViewModel (bookedLine, bookingState, itinerarySettleTime, oneWayLine, plannedTrip, problemLine, rejectedLine, returnLine, submit)
import PUI (action, atCase, blank, debounced, mvu, state)
import PUI.Web ((<+>), choice, inCase, shown, shownWhen, staticText, text)
import PUI.Web.HTML (body, button, div, input, label, output, p, select)
import QualifiedDo.Semigroupoid as Semigroupoid

flightBookerHTML :: Effect Unit
flightBookerHTML =
  body $ div $ Semigroupoid.do
    ( Semigroupoid.do
      p ( label $ Semigroupoid.do
        (staticText @"Flight type ") # shown
        select @"Flight type" (choice @"one-way" <+> choice @"return") )
      p ( label $ Semigroupoid.do
        (staticText @"Start date (DD.MM.YYYY) ") # shown
        input @"Start date (DD.MM.YYYY)" "text" )
      p ( label $ Semigroupoid.do
        (staticText @"Return date (DD.MM.YYYY) ") # shown
        input @"Return date (DD.MM.YYYY)" "text" ) # inCase @"return" _."Flight type"
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
      output @"booked" bookedLine
      output @"rejected" rejectedLine
