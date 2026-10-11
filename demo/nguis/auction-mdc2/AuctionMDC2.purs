module AuctionMDC2 (auctionMDC2) where

import Prelude ((#), ($), Unit)

import AuctionViewModel (bidLine, openingBid, raiseTop, topLine)
import Effect (Effect)
import PUI (looped, settled, suffixed, with)
import PUI.Pipeline as Pipeline
import PUI.Web (text)
import PUI.Web.MDC2 (body, body2, headline6, sliderLive)

auctionMDC2 :: Effect Unit
auctionMDC2 =
  body $ Pipeline.do
    body2 $ text bidLine
    sliderLive @"Your bid ($)" {} # settled raiseTop # suffixed (headline6 $ text topLine)
  # looped
    @( "Your bid ($)" :: { current :: Number, min :: Number, max :: Number, step :: [ discrete :: Number, continuous :: {} ] }
     , top :: Number
     ) # with openingBid
