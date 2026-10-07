module TicketDispenserViewModel (emptyQueue, firstTicketHint, issue, noTicketLine, servingLine, ticketLine, ticketTakenLine) where

import Prelude ((+), (<>), show)

emptyQueue :: { display :: [ serving :: { number :: Int }, waiting :: {} ], next :: Int }
emptyQueue = { display: .waiting {}, next: 1 }

issue :: { display :: [ serving :: { number :: Int }, waiting :: {} ], next :: Int } -> { display :: [ serving :: { number :: Int }, waiting :: {} ], next :: Int }
issue { next } = { display: .serving { number: next }, next: next + 1 }

ticketTakenLine :: { display :: [ serving :: { number :: Int }, waiting :: {} ], next :: Int } -> String
ticketTakenLine { next } = "Took ticket #" <> show next

ticketLine :: { number :: Int } -> String
ticketLine { number } = "#" <> show number

servingLine :: { number :: Int } -> String
servingLine { number } = "Now serving ticket " <> show number <> "."

noTicketLine :: {} -> String
noTicketLine _ = "—"

firstTicketHint :: {} -> String
firstTicketHint _ = "Press the button to draw the first ticket."
