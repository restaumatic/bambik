module ParcelMDC3 (parcelMDC3) where

import Prelude (Unit, (#), ($))

import Effect (Effect)
import ParcelViewModel (draftParcel, parcelLine)
import PUI (PUI, looped, subStrong, with)
import PUI.Web.MDC3 (body, bodyLarge, filledTextField)
import PUI.Web (Web, shown, text)
import QualifiedDo.Semigroupoid as Semigroupoid

parcelMDC3 :: Effect Unit
parcelMDC3 =
  body $
    ( Semigroupoid.do
      filledTextField @"Recipient" {}
      addressForm # subStrong
      ( bodyLarge $ text parcelLine ) # shown
    ) # looped @( "Recipient" :: String, "Street" :: String, "City" :: String ) # with draftParcel

addressForm :: PUI Web { "Street" :: String, "City" :: String } { "Street" :: String, "City" :: String }
addressForm = Semigroupoid.do
  filledTextField @"Street" {}
  filledTextField @"City" {}
