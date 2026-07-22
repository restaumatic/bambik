module Auction (auction) where

import Prelude ((#), ($), (<>), Unit, identity, max, show)

import Data.Profunctor (lcmap)
import Data.Profunctor.Row.RecordToRecord (feedback)
import Effect (Effect)
import PUI (asField, forField, mvu, projection, seeded, tapped)
import PUI.HTML (body, text)
import PUI.MDC (body2, card, elevation20, headline6, sliderLive)
import QualifiedDo.Semigroupoid as Semigroupoid

type Auction = { bid :: Number }

auction :: Effect Unit
auction =
  body $
    elevation20 $
      card { caption: "Live Auction" } $ ( Semigroupoid.do
          body2 text # projection bidLine # forField @"bid" # tapped
          sliderLive { label: "Your bid ($)", min: minBid, max: maxBid, step: bidStep } # asField @"bid"
          ( Semigroupoid.do
              seeded noBids
              lcmap raiseTop identity
              headline6 text # projection topLine # tapped) # feedback
      ) # mvu openingBid

raiseTop :: { bid :: Number, top :: Number } -> { bid :: Number, top :: Number }
raiseTop r = { bid: r.bid, top: max r.bid r.top }

topLine :: { bid :: Number, top :: Number } -> String
topLine r = "Highest bid so far: $" <> show r.top

bidLine :: Number -> String
bidLine b = "Your current bid: $" <> show b

noBids :: { bid :: Number, top :: Number }
noBids = { bid: 0.0, top: 0.0 }

openingBid :: Auction
openingBid = { bid: 0.0 }

minBid :: Number
minBid = 0.0

maxBid :: Number
maxBid = 1000.0

bidStep :: Number
bidStep = 10.0
