module AuctionLogic (noBids, openingBid, raiseTop) where

import Prelude (max)

import Data.Maybe (Maybe(..))

openingBid :: { bid :: { current :: Number, min :: Number, max :: Number, step :: Maybe Number } }
openingBid = { bid: biddingRange }

noBids :: { bid :: { current :: Number, min :: Number, max :: Number, step :: Maybe Number }, top :: Number }
noBids = { bid: biddingRange, top: 0.0 }

raiseTop :: { bid :: { current :: Number, min :: Number, max :: Number, step :: Maybe Number }, top :: Number } -> { bid :: { current :: Number, min :: Number, max :: Number, step :: Maybe Number }, top :: Number }
raiseTop { bid, top } = { bid, top: max bid.current top }

biddingRange :: { current :: Number, min :: Number, max :: Number, step :: Maybe Number }
biddingRange = { current: 0.0, min: 0.0, max: 1000.0, step: Just 10.0 }
