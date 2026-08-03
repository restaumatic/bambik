module TicketDispenserMDC3 (ticketDispenserMDC3) where

import Prelude (Unit, const, identity, show, (#), ($))

import Data.Profunctor.Row.RecordToRecord as RecordToRecord
import Data.Profunctor.Row.VariantToRecord (reelE, unfolding)
import Effect (Effect)
import PUI (asCase, atField, displayed, forField, mvu, updated)
import PUI.Web.HTML (atCase, body, staticText, text)
import PUI.Web.MDC3 (bodyMedium, button, card, elevation5, displaySmall)
import QualifiedDo.Semigroupoid as Semigroupoid
import TicketDispenserLogic (emptyQueue, firstTicket, issue, nextTicket)

ticketDispenserMDC3 :: Effect Unit
ticketDispenserMDC3 =
  body $
    elevation5 $
      card { caption: "Ticket Dispenser" } $ ( Semigroupoid.do
          displaySmall ( Semigroupoid.do
              (staticText "—" # atCase @"waiting" identity # atField @"display") # displayed
              ( ( RecordToRecord.do
                  staticText "#"
                  text # forField @"number" show ) # atCase @"serving" identity # atField @"display" ) # displayed )
          bodyMedium ( Semigroupoid.do
              (staticText "Press the button to draw the first ticket." # atCase @"waiting" identity # atField @"display") # displayed
              ( ( RecordToRecord.do
                  staticText "Now serving ticket "
                  text # forField @"number" show
                  staticText "." ) # atCase @"serving" identity # atField @"display" ) # displayed )
          ( Semigroupoid.do
              button { label: "Take a number" } # asCase @"take"
              (reelE issue nextTicket identity) # unfolding @"resume" firstTicket) # updated const
      ) # mvu emptyQueue
