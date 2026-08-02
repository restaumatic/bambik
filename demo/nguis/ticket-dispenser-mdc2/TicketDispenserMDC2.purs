module TicketDispenserMDC2 (ticketDispenserMDC2) where

import Prelude (Unit, const, identity, show, (#), ($), (+))

import Data.Either (Either(..))
import Data.Profunctor.Row.RecordToRecord as RecordToRecord
import Data.Profunctor.Row.VariantToRecord (reelE, unfolding)
import Data.Tuple (Tuple(..))
import Data.Variant (match)
import Effect (Effect)
import PUI (asCase, atField, displayed, forField, mvu, updated)
import PUI.Web.HTML (atCase, body, staticText, text)
import PUI.Web.MDC2 (body2, button, card, elevation20, headline3)
import QualifiedDo.Semigroupoid as Semigroupoid

ticketDispenserMDC2 :: Effect Unit
ticketDispenserMDC2 =
  body $
    elevation20 $
      card { caption: "Ticket Dispenser" } $ ( Semigroupoid.do
          headline3 ( Semigroupoid.do
              (staticText "—" # atCase @"waiting" identity # atField @"display") # displayed
              ( ( RecordToRecord.do
                  staticText "#"
                  text # forField @"number" show ) # atCase @"serving" identity # atField @"display" ) # displayed )
          body2 ( Semigroupoid.do
              (staticText "Press the button to draw the first ticket." # atCase @"waiting" identity # atField @"display") # displayed
              ( ( RecordToRecord.do
                  staticText "Now serving ticket "
                  text # forField @"number" show
                  staticText "." ) # atCase @"serving" identity # atField @"display" ) # displayed )
          ( Semigroupoid.do
              button { label: "Take a number" } # asCase @"take"
              (reelE issue nextTicket identity) # unfolding @"resume" firstTicket) # updated const
      ) # mvu emptyQueue

issue ::
  [ take :: { display :: [ waiting :: {}, serving :: { number :: Int } ] }
  , resume :: { next :: Int }
  ]
  -> Either { display :: [ waiting :: {}, serving :: { number :: Int } ] } { next :: Int }
issue = match { take: Left, resume: Right }

nextTicket :: forall a. Tuple a { next :: Int } -> { display :: [ waiting :: {}, serving :: { number :: Int } ], next :: Int }
nextTicket (Tuple _ { next }) = { display: .serving { number: next }, next: next + 1 }

firstTicket :: { next :: Int }
firstTicket = { next: 1 }

emptyQueue :: { display :: [ waiting :: {}, serving :: { number :: Int } ] }
emptyQueue = { display: .waiting {} }
