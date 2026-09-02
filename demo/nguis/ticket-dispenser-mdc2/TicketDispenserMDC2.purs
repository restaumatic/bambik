module TicketDispenserMDC2 (ticketDispenserMDC2) where

import Prelude (Unit, const, identity, (#), ($))

import Data.Lens.Reel (reelE)
import Data.Profunctor.Row.VariantToRecord (unfolding)
import Effect (Effect)
import PUI (forProperty, mvu, updated)
import PUI.Web.HTML (shownWhen, body, staticText, text)
import PUI.Web.MDC2 (body2, button, card, elevation20, headline3)
import QualifiedDo.Category as Category
import TicketDispenserLogic (displayOf, emptyQueue, firstTicket, issue, nextTicket)

ticketDispenserMDC2 :: Effect Unit
ticketDispenserMDC2 =
  body $
    elevation20 $
      card $ ( Category.do
          headline3 ( Category.do
              (staticText "—") # shownWhen @"waiting" displayOf
              (text @"ticketLine" # forProperty) # shownWhen @"serving" displayOf )
          body2 ( Category.do
              (staticText "Press the button to draw the first ticket.") # shownWhen @"waiting" displayOf
              (text @"servingLine" # forProperty) # shownWhen @"serving" displayOf )
          ( Category.do
              button @"Take a number" {}
              (reelE issue nextTicket identity) # unfolding @"resume" firstTicket ) # updated const
      ) # mvu emptyQueue
