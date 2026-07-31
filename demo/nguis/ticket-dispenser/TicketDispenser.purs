module TicketDispenser (ticketDispenser) where

import Prelude ((#), ($), (+), (==), Unit, const, identity, show)

import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Profunctor (dimap, lcmap)
import Data.Profunctor.Row.RecordToRecord as RecordToRecord
import Data.Profunctor.Row.VariantToRecord (retain, unfolding)
import Data.Tuple (Tuple(..))
import Data.Variant (match)
import Effect (Effect)
import PUI (asCase, displayed, forField, mvu, projection, updates)
import PUI.HTML (body, provided, staticText, text)
import PUI.MDC2 (body2, button, card, elevation20, headline3)
import QualifiedDo.Semigroupoid as Semigroupoid

ticketDispenser :: Effect Unit
ticketDispenser =
  body $
    elevation20 $
      card { caption: "Ticket Dispenser" } $ ( Semigroupoid.do
          headline3 ( Semigroupoid.do
              staticText "—" # provided # lcmap beforeFirstTicket # displayed
              ( RecordToRecord.do
                  staticText "#"
                  text # projection show # forField @"serving" ) # provided # lcmap afterFirstTicket # displayed )
          body2 ( Semigroupoid.do
              staticText "Press the button to draw the first ticket." # provided # lcmap beforeFirstTicket # displayed
              ( RecordToRecord.do
                  staticText "Now serving ticket "
                  text # projection show # forField @"serving"
                  staticText "." ) # provided # lcmap afterFirstTicket # displayed )
          ( Semigroupoid.do
              button { label: "Take a number" } # asCase @"take"
              (retain identity # dimap issue nextTicket) # unfolding @"resume" firstTicket) # updates const
      ) # mvu emptyQueue

issue ::
  [ take :: { serving :: Int }
  , resume :: { next :: Int }
  ]
  -> Either { serving :: Int } { next :: Int }
issue = match { take: Left, resume: Right }

nextTicket :: forall a. Tuple a { next :: Int } -> { serving :: Int, next :: Int }
nextTicket (Tuple _ { next }) = { serving: next, next: next + 1 }

firstTicket :: { next :: Int }
firstTicket = { next: 1 }

beforeFirstTicket :: { serving :: Int } -> Maybe {}
beforeFirstTicket { serving } = if serving == 0 then Just {} else Nothing

afterFirstTicket :: { serving :: Int } -> Maybe { serving :: Int }
afterFirstTicket q@{ serving } = if serving == 0 then Nothing else Just q

emptyQueue :: { serving :: Int }
emptyQueue = { serving: 0 }
