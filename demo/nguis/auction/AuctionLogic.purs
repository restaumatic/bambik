module AuctionLogic (bidLine, noBids, openingBid, raiseTop, topLine) where

import Prelude (max, (<>))

import Data.Number.Format (fixed, toStringWith)

openingBid :: { "Your bid ($)" :: { current :: Number, min :: Number, max :: Number, step :: [ discrete :: Number, continuous :: {} ] } }
openingBid = { "Your bid ($)": biddingRange }

noBids :: Number
noBids = 0.0

bidLine :: forall r1. { "Your bid ($)" :: { current :: Number, min :: Number, max :: Number, step :: [ discrete :: Number, continuous :: {} ] } | r1 } -> String
bidLine r = "Your current bid: $" <> dollars r."Your bid ($)".current

topLine :: forall r1. { top :: Number | r1 } -> String
topLine r = "Highest bid so far: $" <> dollars r.top

raiseTop :: forall r1. { "Your bid ($)" :: { current :: Number, min :: Number, max :: Number, step :: [ discrete :: Number, continuous :: {} ] }, top :: Number | r1 } -> { "Your bid ($)" :: { current :: Number, min :: Number, max :: Number, step :: [ discrete :: Number, continuous :: {} ] }, top :: Number | r1 }
raiseTop r = r { top = max r."Your bid ($)".current r.top }

dollars :: Number -> String
dollars = toStringWith (fixed 0)

biddingRange :: { current :: Number, min :: Number, max :: Number, step :: [ discrete :: Number, continuous :: {} ] }
biddingRange = { current: 0.0, min: 0.0, max: 1000.0, step: .discrete 10.0 }
