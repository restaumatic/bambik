module AuctionMDC2 (auctionMDC2) where

import Prelude (identity, (#), ($), Unit)

import AuctionLogic (bidText, noBids, openingBid, raiseTop, topText)
import Data.Profunctor.Row.RecordToRecord (feedback)
import Effect (Effect)
import PUI (mvu, settled)
import PUI.Web.HTML (body, shown)
import PUI.Web.MDC2 (body2, card, elevation20, headline6, sliderLive)
import QualifiedDo.Semigroupoid as Semigroupoid

auctionMDC2 :: Effect Unit
auctionMDC2 =
  body $
    elevation20 $
      card $ ( Semigroupoid.do
          body2 (shown @"Your bid ($)" bidText)
          sliderLive @"Your bid ($)" {}
          ( Semigroupoid.do
              identity # settled raiseTop
              headline6 (shown @"top" topText) ) # feedback noBids
      ) # mvu openingBid
