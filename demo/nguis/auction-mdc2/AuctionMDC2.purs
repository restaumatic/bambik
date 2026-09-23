module AuctionMDC2 (auctionMDC2) where

import Prelude ((#), ($), Unit)

import AuctionLogic (bidLine, noBids, openingBid, raiseTop, topLine)
import Data.Profunctor.Row.RecordToRecord (feedback)
import Effect (Effect)
import PUI (mvu, settled)
import PUI.Web.HTML (shown, text)
import PUI.Web.MDC2 (body, body2, card, headline6, sliderLive)
import QualifiedDo.Semigroupoid as Semigroupoid

auctionMDC2 :: Effect Unit
auctionMDC2 =
  body $
    card $ ( Semigroupoid.do
      ( body2 $ text bidLine ) # shown
      ( Semigroupoid.do
        sliderLive @"Your bid ($)" {} # settled raiseTop
        ( headline6 $ text topLine ) # shown ) # feedback noBids
    ) # mvu openingBid
