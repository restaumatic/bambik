module Auction.MDC2 (auction) where

import Prelude ((#), ($), Unit)

import Auction (bidLine, openingBid, raiseTop, topLine)
import Effect (Effect)
import PUI (looped, settled, with)
import PUI.Web (text)
import PUI.Web.MDC2 (body, body2, headline6, sliderLive)
import QualifiedDo.Semigroupoid as Semigroupoid

auction :: Effect Unit
auction =
  body $ Semigroupoid.do
    body2 $ text bidLine
    sliderLive @"Your bid ($)" # settled raiseTop
    headline6 $ text topLine
  # looped # with openingBid
