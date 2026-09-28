module TicketDispenserMDC3 (ticketDispenserMDC3) where

import Prelude (Unit, const, identity, (#), ($))

import Data.Profunctor.Row.VariantToRecord (unfolding)
import Effect (Effect)
import PUI (mvu, updated)
import PUI.Web (shownWhen, staticText, text)
import PUI.Web.MDC3 (body, bodyMedium, button, displaySmall)
import QualifiedDo.Semigroupoid as Semigroupoid
import TicketDispenserLogic (displayOf, emptyQueue, firstTicket, servingLine, ticketIssuance, ticketLine)

ticketDispenserMDC3 :: Effect Unit
ticketDispenserMDC3 =
  body $
    ( Semigroupoid.do
      displaySmall ( Semigroupoid.do
        (staticText "—") # shownWhen @"waiting" displayOf
        (text ticketLine) # shownWhen @"serving" displayOf )
      bodyMedium ( Semigroupoid.do
        (staticText "Press the button to draw the first ticket.") # shownWhen @"waiting" displayOf
        (text servingLine) # shownWhen @"serving" displayOf )
      ( Semigroupoid.do
        button @"Take a number" {}
        ticketIssuance identity # unfolding @"resume" firstTicket ) # updated const
    ) # mvu emptyQueue
