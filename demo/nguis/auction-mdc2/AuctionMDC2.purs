module AuctionMDC2 (auctionMDC2) where

import Prelude (identity, (#), ($), (<<<), Unit, show)

import AuctionLogic (noBids, openingBid, raiseTop)
import Data.Profunctor.Row.RecordToRecord (feedback)
import Data.Profunctor.Row.RecordToRecord as RecordToRecord
import Effect (Effect)
import PUI (projection, mvu, settled, tapped)
import PUI.Web.HTML (body, staticText, text)
import PUI.Web.MDC2 (body2, card, elevation20, headline6, sliderLive)
import QualifiedDo.Semigroupoid as Semigroupoid

auctionMDC2 :: Effect Unit
auctionMDC2 =
  body $
    elevation20 $
      card $ ( Semigroupoid.do
          body2 ( RecordToRecord.do
              staticText "Your current bid: $"
              text @"Your bid ($)" # projection (show <<< _.current) ) # tapped
          sliderLive @"Your bid ($)" {}
          ( Semigroupoid.do
              identity # settled raiseTop
              headline6 ( RecordToRecord.do
                  staticText "Highest bid so far: $"
                  text @"top" # projection show ) # tapped) # feedback noBids
      ) # mvu openingBid
