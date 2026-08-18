module ParcelMDC3 (parcelMDC3) where

import Prelude (Unit, (#), ($))

import Data.Profunctor.Row.RecordToRecord as RecordToRecord
import Effect (Effect)
import ParcelLogic (draftParcel)
import PUI (PUI, completed, subStrong, mvu, tapped)
import PUI.Web.HTML (body, staticText, text)
import PUI.Web.MDC3 (bodyLarge, card, elevation5, filledTextField)
import PUI.Web (Web)
import QualifiedDo.Semigroupoid as Semigroupoid

parcelMDC3 :: Effect Unit
parcelMDC3 =
  body $
    elevation5 $
      card { caption: "Parcel" } $ ( Semigroupoid.do
          filledTextField @"Recipient" {} # completed
          addressForm # subStrong
          bodyLarge ( RecordToRecord.do
              text @"Recipient"
              staticText " · "
              text @"Street"
              staticText " · "
              text @"City" ) # tapped
      ) # mvu draftParcel

addressForm :: PUI Web { "Street" :: String, "City" :: String } { "Street" :: String, "City" :: String }
addressForm = RecordToRecord.do
  filledTextField @"Street" {}
  filledTextField @"City" {}
