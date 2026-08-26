module TicketDispenserMDC3 (ticketDispenserMDC3) where

import Prelude (Unit, const, identity, show, (#), ($))

import Data.Profunctor.Row.RecordToRecord as RecordToRecord
import Data.Lens.Reel (reelE)
import Data.Profunctor.Row.VariantToRecord (unfolding)
import Effect (Effect)
import PUI (projection, mvu, updated)
import PUI.Web.HTML (shownCase, body, staticText, text)
import PUI.Web.MDC3 (bodyMedium, button, card, elevation5, displaySmall)
import QualifiedDo.Semigroupoid as Pipeline
import TicketDispenserLogic (displayOf, emptyQueue, firstTicket, issue, nextTicket)

ticketDispenserMDC3 :: Effect Unit
ticketDispenserMDC3 =
  body $
    elevation5 $
      card $ ( Pipeline.do
          displaySmall ( Pipeline.do
              (staticText "—") # shownCase @"waiting" displayOf
              ( RecordToRecord.do
                  staticText "#"
                  text @"number" # projection show ) # shownCase @"serving" displayOf )
          bodyMedium ( Pipeline.do
              (staticText "Press the button to draw the first ticket.") # shownCase @"waiting" displayOf
              ( RecordToRecord.do
                  staticText "Now serving ticket "
                  text @"number" # projection show
                  staticText "." ) # shownCase @"serving" displayOf )
          ( Pipeline.do
              button @"Take a number" {}
              (reelE issue nextTicket identity) # unfolding @"resume" firstTicket) # updated const
      ) # mvu emptyQueue
