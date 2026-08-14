module TicketDispenserMDC2 (ticketDispenserMDC2) where

import Prelude (Unit, const, identity, show, (#), ($))

import Data.Profunctor.Row.RecordToRecord as RecordToRecord
import Data.Lens.Reel (reelE)
import Data.Profunctor.Row.VariantToRecord (unfolding)
import Effect (Effect)
import PUI (asCase, atField, displayed, forField, mvu, updated)
import PUI.Web.HTML (providedCase, body, staticText, text)
import PUI.Web.MDC2 (body2, button, card, elevation20, headline3)
import QualifiedDo.Semigroupoid as Semigroupoid
import TicketDispenserLogic (emptyQueue, firstTicket, issue, nextTicket)

ticketDispenserMDC2 :: Effect Unit
ticketDispenserMDC2 =
  body $
    elevation20 $
      card { caption: "Ticket Dispenser" } $ ( Semigroupoid.do
          headline3 ( Semigroupoid.do
              (staticText "—" # providedCase @"waiting" identity # atField @"display") # displayed
              ( ( RecordToRecord.do
                  staticText "#"
                  text # forField @"number" show ) # providedCase @"serving" identity # atField @"display" ) # displayed )
          body2 ( Semigroupoid.do
              (staticText "Press the button to draw the first ticket." # providedCase @"waiting" identity # atField @"display") # displayed
              ( ( RecordToRecord.do
                  staticText "Now serving ticket "
                  text # forField @"number" show
                  staticText "." ) # providedCase @"serving" identity # atField @"display" ) # displayed )
          ( Semigroupoid.do
              button { label: "Take a number" } # asCase @"clicked" @"take"
              (reelE issue nextTicket identity) # unfolding @"resume" firstTicket) # updated const
      ) # mvu emptyQueue
