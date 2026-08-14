module ParcelMDC3 (parcelMDC3) where

import Prelude (identity, (#), ($), Unit)

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
          filledTextField @"recipient" {} # completed
          addressForm # subStrong
          bodyLarge ( RecordToRecord.do
              text @"recipient"
              staticText " · "
              text @"street"
              staticText " · "
              text @"city" ) # tapped
      ) # mvu draftParcel

addressForm :: PUI Web { street :: String, city :: String } { street :: String, city :: String }
addressForm = RecordToRecord.do
  filledTextField @"street" {}
  filledTextField @"city" {}
