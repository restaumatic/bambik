module TicketDispenserMDC2 (ticketDispenserMDC2) where

import Prelude ((#), ($), Unit, identity)

import Data.Profunctor.Row.VariantToRecord (unfolding)
import Data.Lens.Reel (reelE)
import Effect (Effect)
import PUI (mvu)
import PUI.Web (shownWhen, text)
import PUI.Web.MDC2 (body, body2, button, headline3)
import QualifiedDo.Semigroupoid as Semigroupoid
import TicketDispenserViewModel (emptyQueue, firstTicket, firstTicketHint, issue, nextTicket, noTicketLine, servingLine, ticketLine)

ticketDispenserMDC2 :: Effect Unit
ticketDispenserMDC2 =
  body $
    ( Semigroupoid.do
      headline3 ( Semigroupoid.do
        (text noTicketLine) # shownWhen @"waiting" _.display
        (text ticketLine) # shownWhen @"serving" _.display )
      body2 ( Semigroupoid.do
        (text firstTicketHint) # shownWhen @"waiting" _.display
        (text servingLine) # shownWhen @"serving" _.display )
      ( Semigroupoid.do
        button @"Take a number" {}
        reelE @{ display :: [ waiting :: {}, serving :: { number :: Int } ] } @{ next :: Int } issue nextTicket identity # unfolding @"resume" @"next" @Int firstTicket )
    ) # mvu @( display :: [ waiting :: {}, serving :: { number :: Int } ] ) emptyQueue
