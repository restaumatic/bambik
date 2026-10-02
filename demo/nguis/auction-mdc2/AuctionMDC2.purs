module AuctionMDC2 (auctionMDC2) where

import Prelude ((#), ($), Unit)

import AuctionViewModel (bidLine, noBids, openingBid, raiseTop, topLine)
import Data.Profunctor.Row.RecordToRecord (feedback)
import Effect (Effect)
import PUI (mvu, settled)
import PUI.Web (shown, text)
import PUI.Web.MDC2 (body, body2, headline6, sliderLive)
import QualifiedDo.Semigroupoid as Semigroupoid

auctionMDC2 :: Effect Unit
auctionMDC2 =
  body $
    ( Semigroupoid.do
      ( body2 $ text bidLine ) # shown
      ( Semigroupoid.do
        sliderLive @"Your bid ($)" {} # settled raiseTop
        ( headline6 $ text topLine ) # shown ) # feedback @"top" @Number noBids
    ) # mvu
      @( "Your bid ($)" :: { current :: Number, min :: Number, max :: Number, step :: [ discrete :: Number, continuous :: {} ] }
       )
      openingBid
