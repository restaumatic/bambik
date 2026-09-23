module TicketDispenserMDC3 (ticketDispenserMDC3) where

import Prelude (Unit, const, identity, (#), ($))

import Data.Profunctor.Row.VariantToRecord (unfolding)
import Effect (Effect)
import PUI (mvu, toCases, updated)
import PUI.Web.HTML (shownWhen, staticText, text)
import PUI.Web.MDC3 (body, bodyMedium, button, card, displaySmall)
import QualifiedDo.Semigroupoid as Semigroupoid
import TicketDispenserLogic (displayOf, emptyQueue, firstTicket, servingLine, ticketIssuance, ticketLine, ticketRequested)

ticketDispenserMDC3 :: Effect Unit
ticketDispenserMDC3 =
  body $
    card $ ( Semigroupoid.do
      displaySmall ( Semigroupoid.do
        (staticText "—") # shownWhen @"waiting" displayOf
        (text ticketLine) # shownWhen @"serving" displayOf )
      bodyMedium ( Semigroupoid.do
        (staticText "Press the button to draw the first ticket.") # shownWhen @"waiting" displayOf
        (text servingLine) # shownWhen @"serving" displayOf )
      ( Semigroupoid.do
        button @"Take a number" {} # toCases ticketRequested
        ticketIssuance identity # unfolding @"resume" firstTicket ) # updated const
    ) # mvu emptyQueue
