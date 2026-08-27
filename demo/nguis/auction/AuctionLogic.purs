module AuctionLogic (bid, dollars, noBids, openingBid, raiseTop) where

import Prelude (max)

import Data.Number.Format (fixed, toStringWith)

openingBid :: { "Your bid ($)" :: { current :: Number, min :: Number, max :: Number, step :: [ discrete :: Number, continuous :: {} ] } }
openingBid = { "Your bid ($)": biddingRange }

noBids :: { "Your bid ($)" :: { current :: Number, min :: Number, max :: Number, step :: [ discrete :: Number, continuous :: {} ] }, top :: Number }
noBids = { "Your bid ($)": biddingRange, top: 0.0 }

raiseTop :: { "Your bid ($)" :: { current :: Number, min :: Number, max :: Number, step :: [ discrete :: Number, continuous :: {} ] }, top :: Number } -> { "Your bid ($)" :: { current :: Number, min :: Number, max :: Number, step :: [ discrete :: Number, continuous :: {} ] }, top :: Number }
raiseTop { "Your bid ($)": offer, top } = { "Your bid ($)": offer, top: max offer.current top }

bid :: { current :: Number, min :: Number, max :: Number, step :: [ discrete :: Number, continuous :: {} ] } -> String
bid { current } = dollars current

dollars :: Number -> String
dollars = toStringWith (fixed 0)

biddingRange :: { current :: Number, min :: Number, max :: Number, step :: [ discrete :: Number, continuous :: {} ] }
biddingRange = { current: 0.0, min: 0.0, max: 1000.0, step: .discrete 10.0 }
