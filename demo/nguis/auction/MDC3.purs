module Auction.MDC3 (auction) where

import Prelude ((#), ($), Unit)

import Auction (bidLine, openingBid, raiseTop, topLine)
import Effect (Effect)
import PUI (looped, settled, with)
import PUI.Web (text)
import PUI.Web.MDC3 (body, bodyMedium, headlineSmall, sliderLive')
import QualifiedDo.Semigroupoid as Semigroupoid

auction :: Effect Unit
auction =
  body $ Semigroupoid.do
    bodyMedium $ text bidLine
    sliderLive' @"Bid" { unit: "USD" } # settled raiseTop
    headlineSmall $ text topLine
  # looped # with openingBid
