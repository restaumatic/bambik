module TicketDispenserMDC2 (ticketDispenserMDC2) where

import Prelude (Unit, const, identity, (#), ($))

import Data.Profunctor.Row.VariantToRecord (unfolding)
import Effect (Effect)
import PUI (mvu, updated)
import PUI.Web (shownWhen, staticText, text)
import PUI.Web.MDC2 (body, body2, button, card, headline3)
import QualifiedDo.Semigroupoid as Semigroupoid
import TicketDispenserLogic (displayOf, emptyQueue, firstTicket, servingLine, ticketIssuance, ticketLine)

ticketDispenserMDC2 :: Effect Unit
ticketDispenserMDC2 =
  body $
    card $ ( Semigroupoid.do
      headline3 ( Semigroupoid.do
        (staticText "—") # shownWhen @"waiting" displayOf
        (text ticketLine) # shownWhen @"serving" displayOf )
      body2 ( Semigroupoid.do
        (staticText "Press the button to draw the first ticket.") # shownWhen @"waiting" displayOf
        (text servingLine) # shownWhen @"serving" displayOf )
      ( Semigroupoid.do
        button @"Take a number" {}
        ticketIssuance identity # unfolding @"resume" firstTicket ) # updated const
    ) # mvu emptyQueue
