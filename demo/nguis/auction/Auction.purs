module Auction (bidLine, openingBid, raiseTop, topLine) where

import Prelude (max, (<>))

import Data.Number.Format (fixed, toStringWith)

openingBid
  :: { "Bid" :: { current :: Number
                         , max :: Number
                         , min :: Number
                         , step :: [ continuous :: {}, discrete :: Number ]
                         }
     , top :: Number
     }
openingBid = { "Bid": biddingRange, top: 0.0 }

bidLine
  :: { "Bid" :: { current :: Number
                         , max :: Number
                         , min :: Number
                         , step :: [ continuous :: {}, discrete :: Number ]
                         }
     , top :: Number
     }
  -> String
bidLine r = "Your current bid: $" <> dollars r."Bid".current

topLine
  :: { "Bid" :: { current :: Number
                         , max :: Number
                         , min :: Number
                         , step :: [ continuous :: {}, discrete :: Number ]
                         }
     , top :: Number
     }
  -> String
topLine r = "Highest bid so far: $" <> dollars r.top

raiseTop
  :: { "Bid" :: { current :: Number
                         , max :: Number
                         , min :: Number
                         , step :: [ continuous :: {}, discrete :: Number ]
                         }
     , top :: Number
     }
  -> { "Bid" :: { current :: Number
                         , max :: Number
                         , min :: Number
                         , step :: [ continuous :: {}, discrete :: Number ]
                         }
     , top :: Number
     }
raiseTop r = r { top = max r."Bid".current r.top }

dollars :: Number -> String
dollars = toStringWith (fixed 0)

biddingRange
  :: { current :: Number
     , min :: Number
     , max :: Number
     , step :: [ discrete :: Number, continuous :: {} ]
     }
biddingRange = { current: 0.0, min: 0.0, max: 1000.0, step: .discrete 10.0 }
