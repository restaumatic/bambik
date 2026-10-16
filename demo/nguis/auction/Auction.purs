module Auction (Auction, Bid, bidLine, openingBid, raiseTop, topLine) where

import Prelude (max, (<>))

import Data.Number.Format (fixed, toStringWith)

type Auction = { "Bid" :: Bid, top :: Number }

type Bid =
  { current :: Number
  , min :: Number
  , max :: Number
  , step :: [ discrete :: Number, continuous :: {} ]
  }

openingBid :: Auction
openingBid = { "Bid": biddingRange, top: 0.0 }

bidLine :: Auction -> String
bidLine r = "Your current bid: $" <> dollars r."Bid".current

topLine :: Auction -> String
topLine r = "Highest bid so far: $" <> dollars r.top

raiseTop :: Auction -> Auction
raiseTop r = r { top = max r."Bid".current r.top }

dollars :: Number -> String
dollars = toStringWith (fixed 0)

biddingRange :: Bid
biddingRange = { current: 0.0, min: 0.0, max: 1000.0, step: .discrete 10.0 }
