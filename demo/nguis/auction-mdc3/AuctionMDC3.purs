module AuctionMDC3 (auctionMDC3) where

import Prelude ((#), ($), Unit)

import AuctionViewModel (bidLine, noBids, openingBid, raiseTop, topLine)
import Data.Profunctor.Row.RecordToRecord (feedback)
import Effect (Effect)
import PUI (mvu, settled)
import PUI.Web (shown, text)
import PUI.Web.MDC3 (body, bodyMedium, headlineSmall, sliderLive)
import QualifiedDo.Semigroupoid as Semigroupoid

auctionMDC3 :: Effect Unit
auctionMDC3 =
  body $
    ( Semigroupoid.do
      ( bodyMedium $ text bidLine ) # shown
      ( Semigroupoid.do
        sliderLive @"Your bid ($)" {} # settled raiseTop
        ( headlineSmall $ text topLine ) # shown ) # feedback @"top" @Number noBids
    ) # mvu openingBid
