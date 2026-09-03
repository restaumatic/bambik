module AuctionLogic (bidLine, noBids, openingBid, raiseTop, topLine) where

import Prelude (max, (<>))

import Data.Number.Format (fixed, toStringWith)

openingBid :: { "Your bid ($)" :: { current :: Number, min :: Number, max :: Number, step :: [ discrete :: Number, continuous :: {} ] } }
openingBid = { "Your bid ($)": biddingRange }

noBids :: { "Your bid ($)" :: { current :: Number, min :: Number, max :: Number, step :: [ discrete :: Number, continuous :: {} ] }, top :: Number }
noBids = { "Your bid ($)": biddingRange, top: 0.0 }

bidLine :: { "Your bid ($)" :: { current :: Number, min :: Number, max :: Number, step :: [ discrete :: Number, continuous :: {} ] } } -> String
bidLine r = "Your current bid: $" <> dollars r."Your bid ($)".current

topLine :: { top :: Number } -> String
topLine r = "Highest bid so far: $" <> dollars r.top

raiseTop :: { "Your bid ($)" :: { current :: Number, min :: Number, max :: Number, step :: [ discrete :: Number, continuous :: {} ] }, top :: Number } -> { "Your bid ($)" :: { current :: Number, min :: Number, max :: Number, step :: [ discrete :: Number, continuous :: {} ] }, top :: Number }
raiseTop r = r { top = max r."Your bid ($)".current r.top }

dollars :: Number -> String
dollars = toStringWith (fixed 0)

biddingRange :: { current :: Number, min :: Number, max :: Number, step :: [ discrete :: Number, continuous :: {} ] }
biddingRange = { current: 0.0, min: 0.0, max: 1000.0, step: .discrete 10.0 }
