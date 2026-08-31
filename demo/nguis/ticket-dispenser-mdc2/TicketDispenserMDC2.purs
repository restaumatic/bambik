module TicketDispenserMDC2 (ticketDispenserMDC2) where

import Prelude (Unit, const, identity, (#), ($))

import Data.Profunctor.Row.RecordToRecord as RecordToRecord
import Data.Lens.Reel (reelE)
import Data.Profunctor.Row.VariantToRecord (unfolding)
import Effect (Effect)
import PUI (mvu, updated)
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
              ( RecordToRecord.do
                  staticText "#"
                  text @"ticketText" ) # shownWhen @"serving" displayOf )
          body2 ( Category.do
              (staticText "Press the button to draw the first ticket.") # shownWhen @"waiting" displayOf
              ( RecordToRecord.do
                  staticText "Now serving ticket "
                  text @"ticketText"
                  staticText "." ) # shownWhen @"serving" displayOf )
          ( Category.do
              button @"Take a number" {}
              (reelE issue nextTicket identity) # unfolding @"resume" firstTicket ) # updated const
      ) # mvu emptyQueue
