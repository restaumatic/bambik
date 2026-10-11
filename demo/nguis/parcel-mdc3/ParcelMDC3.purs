module ParcelMDC3 (parcelMDC3) where

import Prelude (Unit, (#), ($))

import Effect (Effect)
import PUI.Pipeline as Pipeline
import ParcelViewModel (draftParcel, parcelLine)
import PUI (PUI, looped, subStrong, suffixed, with)
import PUI.Web.MDC3 (body, bodyLarge, filledTextField)
import PUI.Web (Web, text)

parcelMDC3 :: Effect Unit
parcelMDC3 =
  body $ Pipeline.do
    filledTextField @"Recipient" {}
    addressForm # subStrong # suffixed (bodyLarge $ text parcelLine)
  # looped @( "Recipient" :: String, "Street" :: String, "City" :: String ) # with draftParcel

addressForm :: PUI Web { "Street" :: String, "City" :: String } { "Street" :: String, "City" :: String }
addressForm = Pipeline.do
  filledTextField @"Street" {}
  filledTextField @"City" {}
