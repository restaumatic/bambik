module Auction (auction) where

import Prelude ((#), ($), Unit, identity, max, show)

import Data.Profunctor (lcmap)
import Data.Profunctor.Row.RecordToRecord (feedback)
import Data.Profunctor.Row.RecordToRecord as RecordToRecord
import Effect (Effect)
import PUI (asField, mvu, projection, seeded, tapped)
import PUI.HTML (body, staticText, text)
import PUI.MDC (body2, card, elevation20, headline6, sliderLive)
import QualifiedDo.Semigroupoid as Semigroupoid

auction :: Effect Unit
auction =
  body $
    elevation20 $
      card { caption: "Live Auction" } $ ( Semigroupoid.do
          body2 ( RecordToRecord.do
              staticText "Your current bid: $"
              text # projection bidText ) # tapped
          sliderLive { label: "Your bid ($)", min: minBid, max: maxBid, step: bidStep } # asField @"bid"
          ( Semigroupoid.do
              seeded noBids
              lcmap raiseTop identity
              headline6 ( RecordToRecord.do
                  staticText "Highest bid so far: $"
                  text # projection topText ) # tapped) # feedback
      ) # mvu openingBid

raiseTop :: { bid :: Number, top :: Number } -> { bid :: Number, top :: Number }
raiseTop r = { bid: r.bid, top: max r.bid r.top }

topText :: { top :: Number } -> String
topText r = show r.top

bidText :: { bid :: Number } -> String
bidText r = show r.bid

noBids :: { bid :: Number, top :: Number }
noBids = { bid: 0.0, top: 0.0 }

openingBid :: { bid :: Number }
openingBid = { bid: 0.0 }

minBid :: Number
minBid = 0.0

maxBid :: Number
maxBid = 1000.0

bidStep :: Number
bidStep = 10.0
