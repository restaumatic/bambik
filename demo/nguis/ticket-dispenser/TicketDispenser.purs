module TicketDispenser (ticketDispenser) where

import Prelude ((#), ($), (+), (<>), (==), Unit, const, identity, show)

import Data.Either (Either(..))
import Data.Profunctor (dimap)
import Data.Profunctor.Row.VariantToRecord (retain, unfolding)
import Data.Tuple (Tuple(..))
import Data.Variant (match)
import Effect (Effect)
import PUI (asCase, forValue, mvu, projection, seeded, tapped, updates)
import PUI.HTML (body, text)
import PUI.MDC (body2, button, card, elevation20, headline3)
import QualifiedDo.Semigroupoid as Semigroupoid

type Queue = { serving :: Int }

ticketDispenser :: Effect Unit
ticketDispenser =
  body $
    elevation20 $
      card { caption: "Ticket Dispenser" } $ ( Semigroupoid.do
          headline3 (text # projection nowServing # forValue) # tapped
          body2 (text # projection hint # forValue) # tapped
          ( Semigroupoid.do
              button { label: "Take a number" } # asCase @"take"
              ( Semigroupoid.do
                  seeded firstTicket
                  retain identity # dimap issue nextTicket
              ) # unfolding @"resume"
          ) # updates const
      ) # mvu emptyQueue

issue ::
  [ take :: Queue
  , resume :: { next :: Int }
  ]
  -> Either Queue { next :: Int }
issue = match { take: Left, resume: Right }

nextTicket :: Tuple Queue { next :: Int } -> { serving :: Int, next :: Int }
nextTicket (Tuple _ state) = { serving: state.next, next: state.next + 1 }

firstTicket ::
  [ take :: Queue
  , resume :: { next :: Int }
  ]
firstTicket = .resume { next: 1 }

nowServing :: Queue -> String
nowServing q = if q.serving == 0 then "—" else "#" <> show q.serving

hint :: Queue -> String
hint q = if q.serving == 0 then "Press the button to draw the first ticket." else "Now serving ticket " <> show q.serving <> "."

emptyQueue :: Queue
emptyQueue = { serving: 0 }
