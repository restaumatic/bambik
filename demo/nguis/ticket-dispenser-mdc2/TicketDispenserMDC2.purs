module TicketDispenserMDC2 (ticketDispenserMDC2) where

import Prelude (Unit, const, identity, show, (#), ($))

import Data.Profunctor.Row.RecordToRecord as RecordToRecord
import Data.Lens.Reel (reelE)
import Data.Profunctor.Row.VariantToRecord (unfolding)
import Effect (Effect)
import PUI (projection, mvu, updated)
import PUI.Web.HTML (shownCase, body, staticText, text)
import PUI.Web.MDC2 (body2, button, card, elevation20, headline3)
import QualifiedDo.Semigroupoid as Semigroupoid
import TicketDispenserLogic (displayOf, emptyQueue, firstTicket, issue, nextTicket)

ticketDispenserMDC2 :: Effect Unit
ticketDispenserMDC2 =
  body $
    elevation20 $
      card $ ( Semigroupoid.do
          headline3 ( Semigroupoid.do
              shownCase @"waiting" displayOf (staticText "—")
              shownCase @"serving" displayOf ( RecordToRecord.do
                  staticText "#"
                  text @"number" # projection show ) )
          body2 ( Semigroupoid.do
              shownCase @"waiting" displayOf (staticText "Press the button to draw the first ticket.")
              shownCase @"serving" displayOf ( RecordToRecord.do
                  staticText "Now serving ticket "
                  text @"number" # projection show
                  staticText "." ) )
          ( Semigroupoid.do
              button @"Take a number" {}
              (reelE issue nextTicket identity) # unfolding @"resume" firstTicket) # updated const
      ) # mvu emptyQueue
