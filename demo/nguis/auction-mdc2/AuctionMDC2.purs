module AuctionMDC2 (auctionMDC2) where

import Prelude ((#), ($), Unit)

import AuctionLogic (bid, dollars, noBids, openingBid, raiseTop)
import Data.Profunctor.Row.RecordToRecord (feedback)
import Data.Profunctor.Row.RecordToRecord as RecordToRecord
import Effect (Effect)
import PUI (mvu, projection, settled)
import PUI.Web.HTML (body, shown, staticText, text)
import PUI.Web.MDC2 (body2, card, elevation20, headline6, sliderLive)
import QualifiedDo.Category as Category

auctionMDC2 :: Effect Unit
auctionMDC2 =
  body $
    elevation20 $
      card $ ( Category.do
          ( body2 $ RecordToRecord.do
              staticText "Your current bid: $"
              text @"Your bid ($)" # projection bid ) # shown
          ( Category.do
              sliderLive @"Your bid ($)" {} # settled raiseTop
              ( headline6 $ RecordToRecord.do
                  staticText "Highest bid so far: $"
                  text @"top" # projection dollars ) # shown ) # feedback noBids
      ) # mvu openingBid
