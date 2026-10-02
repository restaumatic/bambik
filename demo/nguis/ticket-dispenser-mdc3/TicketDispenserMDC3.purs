module TicketDispenserMDC3 (ticketDispenserMDC3) where

import Prelude (Unit, const, identity, (#), ($))

import Data.Profunctor.Row.VariantToRecord (unfolding)
import Data.Lens.Reel (reelE)
import Effect (Effect)
import PUI (mvu, state, updated)
import PUI.Web (shownWhen, text)
import PUI.Web.MDC3 (body, bodyMedium, button, displaySmall)
import QualifiedDo.Semigroupoid as Semigroupoid
import TicketDispenserViewModel (emptyQueue, firstTicket, firstTicketHint, issue, nextTicket, noTicketLine, servingLine, ticketLine)

ticketDispenserMDC3 :: Effect Unit
ticketDispenserMDC3 =
  body $
    ( Semigroupoid.do
      state @"display" @[ waiting :: {}, serving :: { number :: Int } ]
      displaySmall ( Semigroupoid.do
        (text noTicketLine) # shownWhen @"waiting" _.display
        (text ticketLine) # shownWhen @"serving" _.display )
      bodyMedium ( Semigroupoid.do
        (text firstTicketHint) # shownWhen @"waiting" _.display
        (text servingLine) # shownWhen @"serving" _.display )
      ( Semigroupoid.do
        button @"Take a number" {}
        reelE @{ display :: [ waiting :: {}, serving :: { number :: Int } ] } @{ next :: Int } issue nextTicket identity # unfolding @"resume" @"next" @Int firstTicket ) # updated const
    ) # mvu emptyQueue
