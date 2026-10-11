module ParcelMDC2 (parcelMDC2) where

import Prelude (Unit, (#), ($))

import Effect (Effect)
import PUI.Pipeline as Pipeline
import ParcelViewModel (draftParcel, parcelLine)
import PUI (PUI, looped, subStrong, suffixed, with)
import PUI.Web.MDC2 (body, body1, filledTextField)
import PUI.Web (Web, text)

parcelMDC2 :: Effect Unit
parcelMDC2 =
  body $ Pipeline.do
    filledTextField @"Recipient" {}
    addressForm # subStrong # suffixed (body1 $ text parcelLine)
  # looped @( "Recipient" :: String, "Street" :: String, "City" :: String ) # with draftParcel

addressForm :: PUI Web { "Street" :: String, "City" :: String } { "Street" :: String, "City" :: String }
addressForm = Pipeline.do
  filledTextField @"Street" {}
  filledTextField @"City" {}
