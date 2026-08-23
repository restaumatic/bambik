module TicketDispenserLogic (displayOf, emptyQueue, firstTicket, issue, nextTicket, servingBig, servingLine) where

import Prelude ((<>), show, (+))

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

servingBig :: { number :: Int } -> String
servingBig { number } = "#" <> show number

servingLine :: { number :: Int } -> String
servingLine { number } = "Now serving ticket " <> show number <> "."

displayOf :: { display :: [ waiting :: {}, serving :: { number :: Int } ] } -> [ waiting :: {}, serving :: { number :: Int } ]
displayOf { display } = display
