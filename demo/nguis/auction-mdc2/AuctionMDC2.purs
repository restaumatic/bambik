module AuctionMDC2 (auctionMDC2) where

import Prelude ((#), ($), Unit)

import AuctionLogic (bidLine, noBids, openingBid, raiseTop, topLine)
import Data.Profunctor.Row.RecordToRecord (feedback)
import Effect (Effect)
import PUI (PUI, mvu, settled)
import PUI.Web (Web)
import PUI.Web.HTML (body, shown, text)
import PUI.Web.MDC2 (body2, card, elevation20, headline6, sliderLive)
import QualifiedDo.Category as Category

auctionMDC2 :: Effect Unit
auctionMDC2 =
  body $
    elevation20 $
      card $ ( Category.do
          ( body2 $ text bidLine ) # shown
          standingBid # feedback noBids
      ) # mvu openingBid

standingBid :: PUI Web { "Your bid ($)" :: { current :: Number, min :: Number, max :: Number, step :: [ discrete :: Number, continuous :: {} ] }, top :: Number } { "Your bid ($)" :: { current :: Number, min :: Number, max :: Number, step :: [ discrete :: Number, continuous :: {} ] }, top :: Number }
standingBid = Category.do
  sliderLive @"Your bid ($)" {} # settled raiseTop
  ( headline6 $ text topLine ) # shown
