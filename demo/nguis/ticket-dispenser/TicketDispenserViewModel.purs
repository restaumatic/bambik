module TicketDispenserViewModel (emptyQueue, firstTicket, firstTicketHint, issue, nextTicket, noTicketLine, servingLine, ticketLine) where

import Prelude ((+), (<>), show)

import Data.Either (Either(..))
import Data.Tuple (Tuple(..))
import Data.Variant (match)

emptyQueue :: { display :: [ serving :: { number :: Int }, waiting :: {} ] }
emptyQueue = { display: .waiting {} }

firstTicket :: Int
firstTicket = 1

issue :: [ "Take a number" :: { display :: [ serving :: { number :: Int }, waiting :: {} ] }, resume :: { next :: Int } ] -> Either { display :: [ serving :: { number :: Int }, waiting :: {} ] } { next :: Int }
issue = match { "Take a number": Left, resume: Right }

nextTicket :: Tuple { display :: [ serving :: { number :: Int }, waiting :: {} ] } { next :: Int } -> { display :: [ serving :: { number :: Int }, waiting :: {} ], next :: Int }
nextTicket (Tuple _ { next }) = { display: .serving { number: next }, next: next + 1 }

ticketLine :: { number :: Int } -> String
ticketLine { number } = "#" <> show number

servingLine :: { number :: Int } -> String
servingLine { number } = "Now serving ticket " <> show number <> "."

noTicketLine :: {} -> String
noTicketLine _ = "—"

firstTicketHint :: {} -> String
firstTicketHint _ = "Press the button to draw the first ticket."
