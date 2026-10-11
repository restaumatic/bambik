module TicketDispenserMDC2 (ticketDispenserMDC2) where

import Prelude ((#), ($), Unit)

import Data.Profunctor.Row.RecordToRecord as RecordToRecord
import Effect (Effect)
import PUI (fold, looped, prefixed, with)
import PUI.Web (paneAt, text)
import PUI.Web.MDC2 (body, body2, button, headline3, snackbar)
import QualifiedDo.Semigroupoid as Semigroupoid
import TicketDispenserViewModel (emptyQueue, firstTicketHint, issue, noTicketLine, servingLine, ticketLine, ticketTakenLine)

ticketDispenserMDC2 :: Effect Unit
ticketDispenserMDC2 =
  body $ Semigroupoid.do
    prefixed ( RecordToRecord.do
      headline3 ( RecordToRecord.do
        (text noTicketLine) # paneAt @"waiting" @"display"
        (text ticketLine) # paneAt @"serving" @"display" )
      body2 ( RecordToRecord.do
        (text firstTicketHint) # paneAt @"waiting" @"display"
        (text servingLine) # paneAt @"serving" @"display" ) ) $ button @"Take a number" {}
    snackbar @"Take a number" ticketTakenLine # fold issue
  # looped
    @( display :: [ waiting :: {}, serving :: { number :: Int } ]
     , next :: Int
     ) # with emptyQueue
