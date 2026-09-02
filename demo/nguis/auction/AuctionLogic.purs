module AuctionLogic (noBids, openingBid, presentAuction, raiseTop) where

import Prelude (max, (<>))

import Data.Number.Format (fixed, toStringWith)

openingBid :: { "Your bid ($)" :: { current :: Number, min :: Number, max :: Number, step :: [ discrete :: Number, continuous :: {} ] }, bidLine :: String }
openingBid = presentAuction { "Your bid ($)": biddingRange, bidLine: "" }

noBids :: { "Your bid ($)" :: { current :: Number, min :: Number, max :: Number, step :: [ discrete :: Number, continuous :: {} ] }, bidLine :: String, top :: Number, topLine :: String }
noBids =
  let standing = raiseTop { "Your bid ($)": biddingRange, top: 0.0, topLine: "" }
  in { "Your bid ($)": biddingRange, bidLine: openingBid.bidLine, top: standing.top, topLine: standing.topLine }

presentAuction :: { "Your bid ($)" :: { current :: Number, min :: Number, max :: Number, step :: [ discrete :: Number, continuous :: {} ] }, bidLine :: String } -> { "Your bid ($)" :: { current :: Number, min :: Number, max :: Number, step :: [ discrete :: Number, continuous :: {} ] }, bidLine :: String }
presentAuction r = r { bidLine = "Your current bid: $" <> dollars r."Your bid ($)".current }

raiseTop :: { "Your bid ($)" :: { current :: Number, min :: Number, max :: Number, step :: [ discrete :: Number, continuous :: {} ] }, top :: Number, topLine :: String } -> { "Your bid ($)" :: { current :: Number, min :: Number, max :: Number, step :: [ discrete :: Number, continuous :: {} ] }, top :: Number, topLine :: String }
raiseTop r = let highest = max r."Your bid ($)".current r.top in r { top = highest, topLine = "Highest bid so far: $" <> dollars highest }

dollars :: Number -> String
dollars = toStringWith (fixed 0)

biddingRange :: { current :: Number, min :: Number, max :: Number, step :: [ discrete :: Number, continuous :: {} ] }
biddingRange = { current: 0.0, min: 0.0, max: 1000.0, step: .discrete 10.0 }
