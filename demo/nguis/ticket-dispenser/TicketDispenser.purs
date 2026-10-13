module TicketDispenser (emptyQueue, issue, servingLine, ticketLine) where

import Prelude ((+), (<>), const, show)

import Data.Variant (match)

emptyQueue :: { display :: [ serving :: { number :: Int }, waiting :: {} ], next :: Int }
emptyQueue = { display: .waiting {}, next: 1 }

issue
  :: { display :: [ serving :: { number :: Int }, waiting :: {} ], next :: Int }
  -> { display :: [ serving :: { number :: Int }, waiting :: {} ], next :: Int }
issue { next } = { display: .serving { number: next }, next: next + 1 }

ticketLine :: { display :: [ serving :: { number :: Int }, waiting :: {} ], next :: Int } -> String
ticketLine { display } = match { serving: \{ number } -> "#" <> show number, waiting: const "" } display

servingLine :: { display :: [ serving :: { number :: Int }, waiting :: {} ], next :: Int } -> String
servingLine { display } = match { serving: \{ number } -> "Now serving ticket " <> show number <> ".", waiting: const "" } display

