module TicketDispenser (Queue, emptyQueue, issue, servingLine, ticketLine) where

import Prelude ((+), (<>), const, show)

import Data.Variant (match)

type Queue = { display :: [ serving :: { number :: Int }, waiting :: {} ], next :: Int }

emptyQueue :: Queue
emptyQueue = { display: .waiting {}, next: 1 }

issue :: Queue -> Queue
issue m = m { display = .serving { number: m.next }, next = m.next + 1 }

ticketLine :: Queue -> String
ticketLine { display } = match { serving: \{ number } -> "#" <> show number, waiting: const "" } display

servingLine :: Queue -> String
servingLine { display } = match { serving: \{ number } -> "Now serving ticket " <> show number <> ".", waiting: const "" } display

