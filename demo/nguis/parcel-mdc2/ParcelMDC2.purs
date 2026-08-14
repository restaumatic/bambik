module ParcelMDC2 (parcelMDC2) where

import Prelude (identity, (#), ($), Unit)

import Data.Profunctor.Row.RecordToRecord as RecordToRecord
import Effect (Effect)
import ParcelLogic (draftParcel)
import PUI (PUI, completed, subStrong, mvu, tapped)
import PUI.Web.HTML (body, staticText, text)
import PUI.Web.MDC2 (body1, card, elevation20, filledTextField)
import PUI.Web (Web)
import QualifiedDo.Semigroupoid as Semigroupoid

parcelMDC2 :: Effect Unit
parcelMDC2 =
  body $
    elevation20 $
      card { caption: "Parcel" } $ ( Semigroupoid.do
          filledTextField @"recipient" { floatingLabel: "Recipient" } # completed
          addressForm # subStrong
          body1 ( RecordToRecord.do
              text @"recipient"
              staticText " · "
              text @"street"
              staticText " · "
              text @"city" ) # tapped
      ) # mvu draftParcel

addressForm :: PUI Web { street :: String, city :: String } { street :: String, city :: String }
addressForm = RecordToRecord.do
  filledTextField @"street" { floatingLabel: "Street" }
  filledTextField @"city" { floatingLabel: "City" }
