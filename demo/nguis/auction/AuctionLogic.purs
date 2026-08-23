module AuctionLogic (bidText, noBids, openingBid, raiseTop, topText) where

import Prelude (max, show, (<>))

import Data.Maybe (Maybe(..))

openingBid :: { "Your bid ($)" :: { current :: Number, min :: Number, max :: Number, step :: Maybe Number } }
openingBid = { "Your bid ($)": biddingRange }

noBids :: { "Your bid ($)" :: { current :: Number, min :: Number, max :: Number, step :: Maybe Number }, top :: Number }
noBids = { "Your bid ($)": biddingRange, top: 0.0 }

raiseTop :: { "Your bid ($)" :: { current :: Number, min :: Number, max :: Number, step :: Maybe Number }, top :: Number } -> { "Your bid ($)" :: { current :: Number, min :: Number, max :: Number, step :: Maybe Number }, top :: Number }
raiseTop { "Your bid ($)": bid, top } = { "Your bid ($)": bid, top: max bid.current top }

biddingRange :: { current :: Number, min :: Number, max :: Number, step :: Maybe Number }
biddingRange = { current: 0.0, min: 0.0, max: 1000.0, step: Just 10.0 }

bidText :: { current :: Number, min :: Number, max :: Number, step :: Maybe Number } -> String
bidText bid = "Your current bid: $" <> show bid.current

topText :: Number -> String
topText top = "Highest bid so far: $" <> show top
