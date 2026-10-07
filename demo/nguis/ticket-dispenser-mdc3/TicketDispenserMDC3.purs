module TicketDispenserMDC3 (ticketDispenserMDC3) where

import Prelude ((#), ($), Unit)

import Effect (Effect)
import PUI (fold, looped, with)
import PUI.Web (shownWhen, text)
import PUI.Web.MDC3 (body, bodyMedium, button, displaySmall, snackbar)
import QualifiedDo.Semigroupoid as Semigroupoid
import TicketDispenserViewModel (emptyQueue, firstTicketHint, issue, noTicketLine, servingLine, ticketLine, ticketTakenLine)

ticketDispenserMDC3 :: Effect Unit
ticketDispenserMDC3 =
  body $
    ( Semigroupoid.do
      displaySmall ( Semigroupoid.do
        (text noTicketLine) # shownWhen @"waiting" _.display
        (text ticketLine) # shownWhen @"serving" _.display )
      bodyMedium ( Semigroupoid.do
        (text firstTicketHint) # shownWhen @"waiting" _.display
        (text servingLine) # shownWhen @"serving" _.display )
      button @"Take a number" {}
      snackbar @"Take a number" ticketTakenLine # fold issue
    ) # looped @( display :: [ waiting :: {}, serving :: { number :: Int } ], next :: Int ) # with emptyQueue
