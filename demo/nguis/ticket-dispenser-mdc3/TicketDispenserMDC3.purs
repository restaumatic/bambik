module TicketDispenserMDC3 (ticketDispenserMDC3) where

import Prelude ((#), ($), Unit)

import Data.Profunctor.Row.RecordToRecord as RecordToRecord
import Effect (Effect)
import PUI (fold, looped, prefixed, unshown, with)
import PUI.Web (paneAt, text)
import PUI.Web.MDC3 (body, bodyMedium, button, displaySmall)
import QualifiedDo.Semigroupoid as Semigroupoid
import TicketDispenserViewModel (emptyQueue, firstTicketHint, issue, noTicketLine, servingLine, ticketLine)

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
    unshown @"Take a number" # fold issue
  # looped
    @( display :: [ waiting :: {}, serving :: { number :: Int } ]
     , next :: Int
     ) # with emptyQueue
