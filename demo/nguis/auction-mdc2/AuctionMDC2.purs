module AuctionMDC2 (auctionMDC2) where

import Prelude ((#), ($), Unit)

import AuctionLogic (bidLine, noBids, openingBid, raiseTop, topLine)
import Data.Profunctor.Row.RecordToRecord (feedback)
import Effect (Effect)
import PUI (mvu, settled)
import PUI.Web.HTML (shown, text)
import PUI.Web.MDC2 (body, body2, card, headline6, sliderLive)
import QualifiedDo.Category as Category

auctionMDC2 :: Effect Unit
auctionMDC2 =
  body $
    card $ ( Category.do
      ( body2 $ text bidLine ) # shown
      ( Category.do
        sliderLive @"Your bid ($)" {} # settled raiseTop
        ( headline6 $ text topLine ) # shown ) # feedback noBids
    ) # mvu openingBid
