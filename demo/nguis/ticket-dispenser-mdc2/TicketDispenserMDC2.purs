module TicketDispenserMDC2 (ticketDispenserMDC2) where

import Prelude ((#), ($), Unit)

import Data.Profunctor.Row.RecordToRecord as RecordToRecord
import Effect (Effect)
import PUI (fold, looped, prefixed, unshown, with)
import PUI.Web (paneAt, text)
import PUI.Web.MDC2 (body, body2, button, headline3)
import QualifiedDo.Semigroupoid as Semigroupoid
import TicketDispenserViewModel (emptyQueue, firstTicketHint, issue, noTicketLine, servingLine, ticketLine)

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
    unshown @"Take a number" # fold issue
  # looped
    @( display :: [ waiting :: {}, serving :: { number :: Int } ]
     , next :: Int
     ) # with emptyQueue
