module ParcelMDC2 (parcelMDC2) where

import Prelude (Unit, (#), ($))

import Effect (Effect)
import ParcelViewModel (draftParcel, parcelLine)
import PUI (PUI, looped, with)
import PUI.Web.MDC2 (body, body1, filledTextField, group)
import PUI.Web (Web, text)
import QualifiedDo.Semigroupoid as Semigroupoid

parcelMDC2 :: Effect Unit
parcelMDC2 =
  body $ Semigroupoid.do
    filledTextField @"Recipient"
    group @"Address" addressForm
    body1 $ text parcelLine
  # looped @( "Recipient" :: String, "Address" :: { "Street" :: String, "City" :: String } ) # with draftParcel

addressForm :: PUI Web { "Street" :: String, "City" :: String } { "Street" :: String, "City" :: String }
addressForm = Semigroupoid.do
  filledTextField @"Street"
  filledTextField @"City"
