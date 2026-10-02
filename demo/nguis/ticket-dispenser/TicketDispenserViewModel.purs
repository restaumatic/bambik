module TicketDispenserViewModel (emptyQueue, firstTicket, firstTicketHint, issue, nextTicket, noTicketLine, servingLine, ticketLine) where

import Prelude ((+), (<>), show)

import Data.Either (Either(..))
import Data.Tuple (Tuple(..))
import Data.Variant (match)

emptyQueue :: { display :: [ waiting :: {}, serving :: { number :: Int } ] }
emptyQueue = { display: .waiting {} }

firstTicket :: Int
firstTicket = 1

issue ::
  [ "Take a number" :: { display :: [ waiting :: {}, serving :: { number :: Int } ] }
  , resume :: { next :: Int }
  ]
  -> Either { display :: [ waiting :: {}, serving :: { number :: Int } ] } { next :: Int }
issue = match { "Take a number": Left, resume: Right }

nextTicket :: forall a. Tuple a { next :: Int } -> { display :: [ waiting :: {}, serving :: { number :: Int } ], next :: Int }
nextTicket (Tuple _ { next }) = { display: .serving { number: next }, next: next + 1 }

ticketLine :: forall r1. { number :: Int | r1 } -> String
ticketLine { number } = "#" <> show number

servingLine :: forall r1. { number :: Int | r1 } -> String
servingLine { number } = "Now serving ticket " <> show number <> "."

noTicketLine :: forall r1. { | r1 } -> String
noTicketLine _ = "—"

firstTicketHint :: forall r1. { | r1 } -> String
firstTicketHint _ = "Press the button to draw the first ticket."
