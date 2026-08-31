module AuctionLogic (noBids, openingBid, presentAuction, raiseTop) where

import Prelude (max)

import Data.Number.Format (fixed, toStringWith)

openingBid :: { "Your bid ($)" :: { current :: Number, min :: Number, max :: Number, step :: [ discrete :: Number, continuous :: {} ] }, bidText :: String }
openingBid = presentAuction { "Your bid ($)": biddingRange, bidText: "" }

noBids :: { "Your bid ($)" :: { current :: Number, min :: Number, max :: Number, step :: [ discrete :: Number, continuous :: {} ] }, bidText :: String, top :: Number, topText :: String }
noBids = { "Your bid ($)": biddingRange, bidText: dollars biddingRange.current, top: 0.0, topText: dollars 0.0 }

presentAuction :: { "Your bid ($)" :: { current :: Number, min :: Number, max :: Number, step :: [ discrete :: Number, continuous :: {} ] }, bidText :: String } -> { "Your bid ($)" :: { current :: Number, min :: Number, max :: Number, step :: [ discrete :: Number, continuous :: {} ] }, bidText :: String }
presentAuction r = r { bidText = dollars r."Your bid ($)".current }

raiseTop :: { "Your bid ($)" :: { current :: Number, min :: Number, max :: Number, step :: [ discrete :: Number, continuous :: {} ] }, top :: Number, topText :: String } -> { "Your bid ($)" :: { current :: Number, min :: Number, max :: Number, step :: [ discrete :: Number, continuous :: {} ] }, top :: Number, topText :: String }
raiseTop r = let highest = max r."Your bid ($)".current r.top in r { top = highest, topText = dollars highest }

dollars :: Number -> String
dollars = toStringWith (fixed 0)

biddingRange :: { current :: Number, min :: Number, max :: Number, step :: [ discrete :: Number, continuous :: {} ] }
biddingRange = { current: 0.0, min: 0.0, max: 1000.0, step: .discrete 10.0 }
