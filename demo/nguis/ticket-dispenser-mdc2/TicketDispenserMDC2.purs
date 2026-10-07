module TicketDispenserMDC2 (ticketDispenserMDC2) where

import Prelude ((#), ($), Unit)

import Effect (Effect)
import PUI (fold, looped, with)
import PUI.Web (shownWhen, text)
import PUI.Web.MDC2 (body, body2, button, headline3, snackbar)
import QualifiedDo.Semigroupoid as Semigroupoid
import TicketDispenserViewModel (emptyQueue, firstTicketHint, issue, noTicketLine, servingLine, ticketLine, ticketTakenLine)

ticketDispenserMDC2 :: Effect Unit
ticketDispenserMDC2 =
  body $
    ( Semigroupoid.do
      headline3 ( Semigroupoid.do
        (text noTicketLine) # shownWhen @"waiting" _.display
        (text ticketLine) # shownWhen @"serving" _.display )
      body2 ( Semigroupoid.do
        (text firstTicketHint) # shownWhen @"waiting" _.display
        (text servingLine) # shownWhen @"serving" _.display )
      button @"Take a number" {}
      snackbar @"Take a number" ticketTakenLine # fold issue
    ) # looped @( display :: [ waiting :: {}, serving :: { number :: Int } ], next :: Int ) # with emptyQueue
