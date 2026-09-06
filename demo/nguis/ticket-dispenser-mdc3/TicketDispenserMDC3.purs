module TicketDispenserMDC3 (ticketDispenserMDC3) where

import Prelude (Unit, const, identity, (#), ($))

import Data.Profunctor.Row.VariantToRecord (unfolding)
import Effect (Effect)
import PUI (mvu, toCases, updated)
import PUI.Web.HTML (shownWhen, body, staticText, text)
import PUI.Web.MDC3 (bodyMedium, button, card, displaySmall, elevation5)
import QualifiedDo.Category as Category
import TicketDispenserLogic (displayOf, emptyQueue, firstTicket, servingLine, ticketIssuance, ticketLine, ticketRequested)

ticketDispenserMDC3 :: Effect Unit
ticketDispenserMDC3 =
  body $
    elevation5 $
      card $ ( Category.do
          displaySmall ( Category.do
              (staticText "—") # shownWhen @"waiting" displayOf
              (text ticketLine) # shownWhen @"serving" displayOf )
          bodyMedium ( Category.do
              (staticText "Press the button to draw the first ticket.") # shownWhen @"waiting" displayOf
              (text servingLine) # shownWhen @"serving" displayOf )
          ( Category.do
              button @"Take a number" {} # toCases ticketRequested
              ticketIssuance identity # unfolding @"resume" firstTicket ) # updated const
      ) # mvu emptyQueue
