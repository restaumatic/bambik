module AuctionMDC2 (auctionMDC2) where

import Prelude (identity, (#), ($), Unit, identity, max, show)

import Data.Maybe (Maybe(..))
import Data.Profunctor.Row.RecordToRecord (feedback)
import Data.Profunctor.Row.RecordToRecord as RecordToRecord
import Effect (Effect)
import PUI (asField, mvu, projected, settled, tapped)
import PUI.HTML (body, staticText, text)
import PUI.MDC2 (body2, card, elevation20, headline6, sliderLive)
import QualifiedDo.Semigroupoid as Semigroupoid

auctionMDC2 :: Effect Unit
auctionMDC2 =
  body $
    elevation20 $
      card { caption: "Live Auction" } $ ( Semigroupoid.do
          body2 ( RecordToRecord.do
              staticText "Your current bid: $"
              text # projected bidText ) # tapped
          sliderLive { label: "Your bid ($)" } # asField @"bid"
          ( Semigroupoid.do
              identity # settled raiseTop
              headline6 ( RecordToRecord.do
                  staticText "Highest bid so far: $"
                  text # projected topText ) # tapped) # feedback noBids
      ) # mvu openingBid

raiseTop :: { bid :: { current :: Number, min :: Number, max :: Number, step :: Maybe Number }, top :: Number } -> { bid :: { current :: Number, min :: Number, max :: Number, step :: Maybe Number }, top :: Number }
raiseTop { bid, top } = { bid, top: max bid.current top }

topText :: { top :: Number } -> String
topText { top } = show top

bidText :: { bid :: { current :: Number, min :: Number, max :: Number, step :: Maybe Number } } -> String
bidText { bid } = show bid.current

noBids :: { bid :: { current :: Number, min :: Number, max :: Number, step :: Maybe Number }, top :: Number }
noBids = { bid: biddingRange, top: 0.0 }

openingBid :: { bid :: { current :: Number, min :: Number, max :: Number, step :: Maybe Number } }
openingBid = { bid: biddingRange }

biddingRange :: { current :: Number, min :: Number, max :: Number, step :: Maybe Number }
biddingRange = { current: 0.0, min: 0.0, max: 1000.0, step: Just 10.0 }
