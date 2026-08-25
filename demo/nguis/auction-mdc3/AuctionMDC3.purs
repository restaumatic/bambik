module AuctionMDC3 (auctionMDC3) where

import Prelude (identity, (#), ($), (<<<), Unit, show)

import AuctionLogic (noBids, openingBid, raiseTop)
import Data.Profunctor.Row.RecordToRecord (feedback)
import Data.Profunctor.Row.RecordToRecord as RecordToRecord
import Effect (Effect)
import PUI (projection, mvu, settled)
import PUI.Web.HTML (shownAs, body, staticText, text)
import PUI.Web.MDC3 (bodyMedium, card, elevation5, headlineSmall, sliderLive)
import QualifiedDo.Semigroupoid as Semigroupoid

auctionMDC3 :: Effect Unit
auctionMDC3 =
  body $
    elevation5 $
      card $ ( Semigroupoid.do
          ( bodyMedium $ RecordToRecord.do
              staticText "Your current bid: $"
              text @"Your bid ($)" # projection (show <<< _.current) ) # shownAs identity
          sliderLive @"Your bid ($)" {}
          ( Semigroupoid.do
              identity # settled raiseTop
              ( headlineSmall $ RecordToRecord.do
                  staticText "Highest bid so far: $"
                  text @"top" # projection show ) # shownAs identity ) # feedback noBids
      ) # mvu openingBid
