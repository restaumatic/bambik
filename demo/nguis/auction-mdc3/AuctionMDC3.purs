module AuctionMDC3 (auctionMDC3) where

import Prelude ((#), ($), Unit)

import AuctionLogic (bid, dollars, noBids, openingBid, raiseTop)
import Data.Profunctor.Row.RecordToRecord (feedback)
import Data.Profunctor.Row.RecordToRecord as RecordToRecord
import Effect (Effect)
import PUI (projection, mvu, settled)
import PUI.Web.HTML (shown, body, staticText, text)
import PUI.Web.MDC3 (bodyMedium, card, elevation5, headlineSmall, sliderLive)
import QualifiedDo.Category as Category

auctionMDC3 :: Effect Unit
auctionMDC3 =
  body $
    elevation5 $
      card $ ( Category.do
          ( bodyMedium $ RecordToRecord.do
              staticText "Your current bid: $"
              text @"Your bid ($)" # projection bid ) # shown
          ( Category.do
              sliderLive @"Your bid ($)" {} # settled raiseTop
              ( headlineSmall $ RecordToRecord.do
                  staticText "Highest bid so far: $"
                  text @"top" # projection dollars ) # shown ) # feedback noBids
      ) # mvu openingBid
