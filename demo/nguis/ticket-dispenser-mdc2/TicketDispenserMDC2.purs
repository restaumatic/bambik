module TicketDispenserMDC2 (ticketDispenserMDC2) where

import Prelude (Unit, const, identity, (#), ($))

import Data.Profunctor.Row.VariantToRecord (unfolding)
import Data.Lens.Reel (reelE)
import Effect (Effect)
import PUI (mvu, updated)
import PUI.Web (shownWhen, text)
import PUI.Web.MDC2 (body, body2, button, headline3)
import QualifiedDo.Semigroupoid as Semigroupoid
import TicketDispenserLogic (displayOf, emptyQueue, firstTicket, firstTicketHint, issue, nextTicket, noTicketLine, servingLine, ticketLine)

ticketDispenserMDC2 :: Effect Unit
ticketDispenserMDC2 =
  body $
    ( Semigroupoid.do
      headline3 ( Semigroupoid.do
        (text noTicketLine) # shownWhen @"waiting" displayOf
        (text ticketLine) # shownWhen @"serving" displayOf )
      body2 ( Semigroupoid.do
        (text firstTicketHint) # shownWhen @"waiting" displayOf
        (text servingLine) # shownWhen @"serving" displayOf )
      ( Semigroupoid.do
        button @"Take a number" {}
        reelE issue nextTicket identity # unfolding @"resume" @"next" firstTicket ) # updated const
    ) # mvu emptyQueue
