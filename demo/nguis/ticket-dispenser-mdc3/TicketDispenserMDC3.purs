module TicketDispenserMDC3 (ticketDispenserMDC3) where

import Prelude ((#), ($), Unit)

import Data.Profunctor.Row.RecordToRecord as RecordToRecord
import Effect (Effect)
import PUI (fold, looped, prefixed, with)
import PUI.Web (paneAt, text)
import PUI.Web.MDC3 (body, bodyMedium, button, displaySmall, snackbar)
import QualifiedDo.Semigroupoid as Semigroupoid
import TicketDispenserViewModel (emptyQueue, firstTicketHint, issue, noTicketLine, servingLine, ticketLine, ticketTakenLine)

ticketDispenserMDC3 :: Effect Unit
ticketDispenserMDC3 =
  body $ Semigroupoid.do
    prefixed ( RecordToRecord.do
      displaySmall ( RecordToRecord.do
        (text noTicketLine) # paneAt @"waiting" @"display"
        (text ticketLine) # paneAt @"serving" @"display" )
      bodyMedium ( RecordToRecord.do
        (text firstTicketHint) # paneAt @"waiting" @"display"
        (text servingLine) # paneAt @"serving" @"display" ) ) $ button @"Take a number" {}
    snackbar @"Take a number" ticketTakenLine # fold issue
  # looped
    @( display :: [ waiting :: {}, serving :: { number :: Int } ]
     , next :: Int
     ) # with emptyQueue
