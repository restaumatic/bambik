module AuctionMDC3 (auctionMDC3) where

import Prelude ((#), ($), Unit)

import AuctionLogic (bidLine, noBids, openingBid, raiseTop, topLine)
import Data.Profunctor.Row.RecordToRecord (feedback)
import Effect (Effect)
import PUI (PUI, mvu, settled)
import PUI.Web (Web)
import PUI.Web.HTML (shown, body, text)
import PUI.Web.MDC3 (bodyMedium, card, elevation5, headlineSmall, sliderLive)
import QualifiedDo.Category as Category

auctionMDC3 :: Effect Unit
auctionMDC3 =
  body $
    elevation5 $
      card $ ( Category.do
          ( bodyMedium $ text bidLine ) # shown
          standingBid # feedback noBids
      ) # mvu openingBid

standingBid :: PUI Web { "Your bid ($)" :: { current :: Number, min :: Number, max :: Number, step :: [ discrete :: Number, continuous :: {} ] }, top :: Number } { "Your bid ($)" :: { current :: Number, min :: Number, max :: Number, step :: [ discrete :: Number, continuous :: {} ] }, top :: Number }
standingBid = Category.do
  sliderLive @"Your bid ($)" {} # settled raiseTop
  ( headlineSmall $ text topLine ) # shown
