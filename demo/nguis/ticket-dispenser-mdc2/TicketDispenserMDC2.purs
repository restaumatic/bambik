module TicketDispenserMDC2 (ticketDispenserMDC2) where

import Prelude (Unit, const, identity, show, (#), ($))

import Data.Profunctor.Row.RecordToRecord as RecordToRecord
import Data.Lens.Reel (reelE)
import Data.Profunctor.Row.VariantToRecord (unfolding)
import Effect (Effect)
import PUI (atField, tapped, projection, mvu, updated)
import PUI.Web.HTML (providedCase, body, staticText, text)
import PUI.Web.MDC2 (body2, button, card, elevation20, headline3)
import QualifiedDo.Semigroupoid as Semigroupoid
import TicketDispenserLogic (emptyQueue, firstTicket, issue, nextTicket)

ticketDispenserMDC2 :: Effect Unit
ticketDispenserMDC2 =
  body $
    elevation20 $
      card $ ( Semigroupoid.do
          headline3 ( Semigroupoid.do
              (staticText "—" # providedCase @"waiting" identity # atField @"display") # tapped
              ( ( RecordToRecord.do
                  staticText "#"
                  text @"number" # projection show ) # providedCase @"serving" identity # atField @"display" ) # tapped )
          body2 ( Semigroupoid.do
              (staticText "Press the button to draw the first ticket." # providedCase @"waiting" identity # atField @"display") # tapped
              ( ( RecordToRecord.do
                  staticText "Now serving ticket "
                  text @"number" # projection show
                  staticText "." ) # providedCase @"serving" identity # atField @"display" ) # tapped )
          ( Semigroupoid.do
              button @"Take a number" {}
              (reelE issue nextTicket identity) # unfolding @"resume" firstTicket) # updated const
      ) # mvu emptyQueue
