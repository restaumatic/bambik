module AuctionMDC3 (auctionMDC3) where

import Prelude ((#), ($), Unit)

import AuctionViewModel (bidLine, openingBid, raiseTop, topLine)
import Effect (Effect)
import PUI (looped, settled, with)
import PUI.Web (shown, text)
import PUI.Web.MDC3 (body, bodyMedium, headlineSmall, sliderLive)
import QualifiedDo.Semigroupoid as Semigroupoid

auctionMDC3 :: Effect Unit
auctionMDC3 =
  body $ Semigroupoid.do
    ( bodyMedium $ text bidLine ) # shown
    sliderLive @"Your bid ($)" {} # settled raiseTop
    ( headlineSmall $ text topLine ) # shown
  # looped
    @( "Your bid ($)" :: { current :: Number, min :: Number, max :: Number, step :: [ discrete :: Number, continuous :: {} ] }
     , top :: Number
     ) # with openingBid
