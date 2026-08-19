module TicketDispenserMDC3 (ticketDispenserMDC3) where

import Prelude (Unit, const, identity, show, (#), ($))

import Data.Profunctor.Row.RecordToRecord as RecordToRecord
import Data.Lens.Reel (reelE)
import Data.Profunctor.Row.VariantToRecord (unfolding)
import Effect (Effect)
import PUI (atField, tapped, projection, mvu, updated)
import PUI.Web.HTML (providedCase, body, staticText, text)
import PUI.Web.MDC3 (bodyMedium, button, card, elevation5, displaySmall)
import QualifiedDo.Semigroupoid as Semigroupoid
import TicketDispenserLogic (emptyQueue, firstTicket, issue, nextTicket)

ticketDispenserMDC3 :: Effect Unit
ticketDispenserMDC3 =
  body $
    elevation5 $
      card $ ( Semigroupoid.do
          displaySmall ( Semigroupoid.do
              (staticText "—" # providedCase @"waiting" identity # atField @"display") # tapped
              ( ( RecordToRecord.do
                  staticText "#"
                  text @"number" # projection show ) # providedCase @"serving" identity # atField @"display" ) # tapped )
          bodyMedium ( Semigroupoid.do
              (staticText "Press the button to draw the first ticket." # providedCase @"waiting" identity # atField @"display") # tapped
              ( ( RecordToRecord.do
                  staticText "Now serving ticket "
                  text @"number" # projection show
                  staticText "." ) # providedCase @"serving" identity # atField @"display" ) # tapped )
          ( Semigroupoid.do
              button @"Take a number" {}
              (reelE issue nextTicket identity) # unfolding @"resume" firstTicket) # updated const
      ) # mvu emptyQueue
