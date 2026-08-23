module AuctionMDC2 (auctionMDC2) where

import Prelude (identity, (#), ($), Unit)

import AuctionLogic (bidLine, noBids, openingBid, raiseTop, topLine)
import Data.Profunctor.Row.RecordToRecord (feedback)
import Effect (Effect)
import PUI (mvu, settled)
import PUI.Web.HTML (body, told)
import PUI.Web.MDC2 (body2, card, elevation20, headline6, sliderLive)
import QualifiedDo.Semigroupoid as Semigroupoid

auctionMDC2 :: Effect Unit
auctionMDC2 =
  body $
    elevation20 $
      card $ ( Semigroupoid.do
          body2 (told bidLine)
          sliderLive @"Your bid ($)" {}
          ( Semigroupoid.do
              identity # settled raiseTop
              headline6 (told topLine) ) # feedback noBids
      ) # mvu openingBid
