module TicketDispenserMDC2 (ticketDispenserMDC2) where

import Prelude (Unit, const, identity, show, (#), ($))

import Data.Profunctor.Row.RecordToRecord as RecordToRecord
import Data.Lens.Reel (reelE)
import Data.Profunctor.Row.VariantToRecord (unfolding)
import Effect (Effect)
import PUI (projection, mvu, updated)
import PUI.Web.HTML (shownCase, body, staticText, text)
import PUI.Web.MDC2 (body2, button, card, elevation20, headline3)
import QualifiedDo.Semigroupoid as Pipeline
import TicketDispenserLogic (displayOf, emptyQueue, firstTicket, issue, nextTicket)

ticketDispenserMDC2 :: Effect Unit
ticketDispenserMDC2 =
  body $
    elevation20 $
      card $ ( Pipeline.do
          headline3 ( Pipeline.do
              (staticText "—") # shownCase @"waiting" displayOf
              ( RecordToRecord.do
                  staticText "#"
                  text @"number" # projection show ) # shownCase @"serving" displayOf )
          body2 ( Pipeline.do
              (staticText "Press the button to draw the first ticket.") # shownCase @"waiting" displayOf
              ( RecordToRecord.do
                  staticText "Now serving ticket "
                  text @"number" # projection show
                  staticText "." ) # shownCase @"serving" displayOf )
          ( Pipeline.do
              button @"Take a number" {}
              (reelE issue nextTicket identity) # unfolding @"resume" firstTicket) # updated const
      ) # mvu emptyQueue
