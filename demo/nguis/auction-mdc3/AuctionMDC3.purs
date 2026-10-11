module AuctionMDC3 (auctionMDC3) where

import Prelude ((#), ($), Unit)

import AuctionViewModel (bidLine, openingBid, raiseTop, topLine)
import Effect (Effect)
import PUI (looped, prefixed, settled, suffixed, with)
import PUI.Web (text)
import PUI.Web.MDC3 (body, bodyMedium, headlineSmall, sliderLive)

auctionMDC3 :: Effect Unit
auctionMDC3 =
  body $
    ( prefixed (bodyMedium $ text bidLine) $ sliderLive @"Your bid ($)" {} # settled raiseTop # suffixed (headlineSmall $ text topLine)
    ) # looped
      @( "Your bid ($)" :: { current :: Number, min :: Number, max :: Number, step :: [ discrete :: Number, continuous :: {} ] }
       , top :: Number
       ) # with openingBid
