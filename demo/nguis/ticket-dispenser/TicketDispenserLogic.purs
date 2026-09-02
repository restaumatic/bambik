module TicketDispenserLogic (displayOf, emptyQueue, firstTicket, issue, nextTicket) where

import Prelude ((+), (<>), show)

import Data.Either (Either(..))
import Data.Tuple (Tuple(..))
import Data.Variant (match)

emptyQueue :: { display :: [ waiting :: {}, serving :: { number :: Int } ] }
emptyQueue = { display: .waiting {} }

firstTicket :: { next :: Int }
firstTicket = { next: 1 }

issue ::
  [ "Take a number" :: { display :: [ waiting :: {}, serving :: { number :: Int } ] }
  , resume :: { next :: Int }
  ]
  -> Either { display :: [ waiting :: {}, serving :: { number :: Int } ] } { next :: Int }
issue = match { "Take a number": Left, resume: Right }

nextTicket :: forall a. Tuple a { next :: Int } -> { display :: [ waiting :: {}, serving :: { number :: Int } ], next :: Int }
nextTicket (Tuple _ { next }) = { display: .serving { number: next }, next: next + 1 }

displayOf :: { display :: [ waiting :: {}, serving :: { number :: Int } ] } -> [ waiting :: {}, serving :: { ticketLine :: String, servingLine :: String } ]
displayOf { display } = match { waiting: \_ -> .waiting {}, serving: \{ number } -> .serving { ticketLine: "#" <> show number, servingLine: "Now serving ticket " <> show number <> "." } } display
