module TicketDispenserLogic (emptyQueue, firstTicket, issue, nextTicket) where

import Prelude ((+))

import Data.Either (Either(..))
import Data.Tuple (Tuple(..))
import Data.Variant (match)

emptyQueue :: { display :: [ waiting :: {}, serving :: { number :: Int } ] }
emptyQueue = { display: .waiting {} }

firstTicket :: { next :: Int }
firstTicket = { next: 1 }

issue ::
  [ take :: { display :: [ waiting :: {}, serving :: { number :: Int } ] }
  , resume :: { next :: Int }
  ]
  -> Either { display :: [ waiting :: {}, serving :: { number :: Int } ] } { next :: Int }
issue = match { take: Left, resume: Right }

nextTicket :: forall a. Tuple a { next :: Int } -> { display :: [ waiting :: {}, serving :: { number :: Int } ], next :: Int }
nextTicket (Tuple _ { next }) = { display: .serving { number: next }, next: next + 1 }
