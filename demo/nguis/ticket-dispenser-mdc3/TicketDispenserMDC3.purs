module TicketDispenserMDC3 (ticketDispenserMDC3) where

import Prelude (Unit, const, identity, (#), ($))

import Data.Profunctor.Row.VariantToRecord (unfolding)
import Data.Lens.Reel (reelE)
import Effect (Effect)
import PUI (mvu, updated)
import PUI.Web (shownWhen, text)
import PUI.Web.MDC3 (body, bodyMedium, button, displaySmall)
import QualifiedDo.Semigroupoid as Semigroupoid
import TicketDispenserLogic (displayOf, emptyQueue, firstTicket, firstTicketHint, issue, nextTicket, noTicketLine, servingLine, ticketLine)

ticketDispenserMDC3 :: Effect Unit
ticketDispenserMDC3 =
  body $
    ( Semigroupoid.do
      displaySmall ( Semigroupoid.do
        (text noTicketLine) # shownWhen @"waiting" displayOf
        (text ticketLine) # shownWhen @"serving" displayOf )
      bodyMedium ( Semigroupoid.do
        (text firstTicketHint) # shownWhen @"waiting" displayOf
        (text servingLine) # shownWhen @"serving" displayOf )
      ( Semigroupoid.do
        button @"Take a number" {}
        reelE issue nextTicket identity # unfolding @"resume" @"next" firstTicket ) # updated const
    ) # mvu emptyQueue
