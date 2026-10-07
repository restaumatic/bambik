module AuctionMDC2 (auctionMDC2) where

import Prelude ((#), ($), Unit)

import AuctionViewModel (bidLine, openingBid, raiseTop, topLine)
import Effect (Effect)
import PUI (looped, settled, with)
import PUI.Web (shown, text)
import PUI.Web.MDC2 (body, body2, headline6, sliderLive)
import QualifiedDo.Semigroupoid as Semigroupoid

auctionMDC2 :: Effect Unit
auctionMDC2 =
  body $
    Semigroupoid.do
      ( body2 $ text bidLine ) # shown
      sliderLive @"Your bid ($)" {} # settled raiseTop
      ( headline6 $ text topLine ) # shown
    # looped
      @( "Your bid ($)" :: { current :: Number, min :: Number, max :: Number, step :: [ discrete :: Number, continuous :: {} ] }
       , top :: Number
       ) # with openingBid
