module TicketDispenser (ticketDispenser) where

import Prelude ((#), ($), (+), (<>), (==), Unit, const, identity, show)

import Data.Either (Either(..))
import Data.Profunctor (dimap)
import Data.Profunctor.Row.VariantToRecord (retain, unfolding)
import Data.Tuple (Tuple(..))
import Data.Variant (match)
import Effect (Effect)
import PUI (asCase, mvu, projection, seeded, tapped, updates)
import PUI.HTML (body, text)
import PUI.MDC (body2, button, card, elevation20, headline3)
import QualifiedDo.Semigroupoid as Semigroupoid

ticketDispenser :: Effect Unit
ticketDispenser =
  body $
    elevation20 $
      card { caption: "Ticket Dispenser" } $ ( Semigroupoid.do
          headline3 text # projection nowServing # tapped
          body2 text # projection hint # tapped
          ( Semigroupoid.do
              button { label: "Take a number" } # asCase @"take"
              ( Semigroupoid.do
                  seeded firstTicket
                  retain identity # dimap issue nextTicket) # unfolding @"resume") # updates const
      ) # mvu emptyQueue

issue ::
  [ take :: { serving :: Int }
  , resume :: { next :: Int }
  ]
  -> Either { serving :: Int } { next :: Int }
issue = match { take: Left, resume: Right }

nextTicket :: forall a. Tuple a { next :: Int } -> { serving :: Int, next :: Int }
nextTicket (Tuple _ state) = { serving: state.next, next: state.next + 1 }

firstTicket ::
  [ take :: { serving :: Int }
  , resume :: { next :: Int }
  ]
firstTicket = .resume { next: 1 }

nowServing :: { serving :: Int } -> String
nowServing q = if q.serving == 0 then "—" else "#" <> show q.serving

hint :: { serving :: Int } -> String
hint q = if q.serving == 0 then "Press the button to draw the first ticket." else "Now serving ticket " <> show q.serving <> "."

emptyQueue :: { serving :: Int }
emptyQueue = { serving: 0 }
