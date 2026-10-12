module ParcelMDC3 (parcelMDC3) where

import Prelude (Unit, (#), ($))

import Effect (Effect)
import ParcelViewModel (draftParcel, parcelLine)
import PUI (PUI, looped, suffixed, with)
import PUI.Web.MDC3 (body, bodyLarge, filledTextField, group)
import PUI.Web (Web, text)
import QualifiedDo.Semigroupoid as Semigroupoid

parcelMDC3 :: Effect Unit
parcelMDC3 =
  body $ Semigroupoid.do
    filledTextField @"Recipient" {}
    group @"Address" addressForm # suffixed (bodyLarge $ text parcelLine)
  # looped @( "Recipient" :: String, "Address" :: { "Street" :: String, "City" :: String } ) # with draftParcel

addressForm :: PUI Web { "Street" :: String, "City" :: String } { "Street" :: String, "City" :: String }
addressForm = Semigroupoid.do
  filledTextField @"Street" {}
  filledTextField @"City" {}
