module TicketDispenserLogic (displayOf, emptyQueue, firstTicket, servingLine, ticketIssuance, ticketLine, ticketRequested) where

import Prelude ((+), (<>), show)

import Data.Either (Either(..))
import Data.Lens.Reel (Reel, reelE)
import Data.Tuple (Tuple(..))
import Data.Variant (match)

emptyQueue :: { display :: [ waiting :: {}, serving :: { number :: Int } ] }
emptyQueue = { display: .waiting {} }

firstTicket :: { next :: Int }
firstTicket = { next: 1 }

ticketRequested :: { display :: [ waiting :: {}, serving :: { number :: Int } ] } -> [ requested :: { display :: [ waiting :: {}, serving :: { number :: Int } ] } ]
ticketRequested = .requested

ticketIssuance :: Reel [ requested :: { display :: [ waiting :: {}, serving :: { number :: Int } ] }, resume :: { next :: Int } ] { display :: [ waiting :: {}, serving :: { number :: Int } ], next :: Int } { display :: [ waiting :: {}, serving :: { number :: Int } ] } { display :: [ waiting :: {}, serving :: { number :: Int } ] }
ticketIssuance = reelE issue nextTicket

issue ::
  [ requested :: { display :: [ waiting :: {}, serving :: { number :: Int } ] }
  , resume :: { next :: Int }
  ]
  -> Either { display :: [ waiting :: {}, serving :: { number :: Int } ] } { next :: Int }
issue = match { requested: Left, resume: Right }

nextTicket :: forall a. Tuple a { next :: Int } -> { display :: [ waiting :: {}, serving :: { number :: Int } ], next :: Int }
nextTicket (Tuple _ { next }) = { display: .serving { number: next }, next: next + 1 }

displayOf :: { display :: [ waiting :: {}, serving :: { number :: Int } ] } -> [ waiting :: {}, serving :: { number :: Int } ]
displayOf { display } = display

ticketLine :: { number :: Int } -> String
ticketLine { number } = "#" <> show number

servingLine :: { number :: Int } -> String
servingLine { number } = "Now serving ticket " <> show number <> "."
