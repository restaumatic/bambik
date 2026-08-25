module ParcelMDC3 (parcelMDC3) where

import Prelude (identity, Unit, (#), ($))

import Data.Profunctor.Row.RecordToRecord as RecordToRecord
import Effect (Effect)
import ParcelLogic (draftParcel)
import PUI (PUI, subStrong, mvu)
import PUI.Web.HTML (shownAs, body, staticText, text)
import PUI.Web.MDC3 (bodyLarge, card, elevation5, filledTextField)
import PUI.Web (Web)
import QualifiedDo.Semigroupoid as Semigroupoid

parcelMDC3 :: Effect Unit
parcelMDC3 =
  body $
    elevation5 $
      card $ ( Semigroupoid.do
          filledTextField @"Recipient" {}
          addressForm # subStrong
          ( bodyLarge $ RecordToRecord.do
              text @"Recipient"
              staticText " · "
              text @"Street"
              staticText " · "
              text @"City" ) # shownAs identity
      ) # mvu draftParcel

addressForm :: PUI Web { "Street" :: String, "City" :: String } { "Street" :: String, "City" :: String }
addressForm = Semigroupoid.do
  filledTextField @"Street" {}
  filledTextField @"City" {}
