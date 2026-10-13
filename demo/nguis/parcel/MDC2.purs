module Parcel.MDC2 (parcel) where

import Prelude (Unit, (#), ($))

import Effect (Effect)
import Parcel (draftParcel, parcelLine)
import PUI (PUI, looped, with)
import PUI.Web.MDC2 (body, body1, filledTextField, group)
import PUI.Web (Web, text)
import QualifiedDo.Semigroupoid as Semigroupoid

parcel :: Effect Unit
parcel =
  body $ Semigroupoid.do
    filledTextField @"Recipient"
    group @"Address" addressForm
    body1 $ text parcelLine
  # looped @( "Recipient" :: String, "Address" :: { "Street" :: String, "City" :: String } ) # with draftParcel

addressForm :: PUI Web { "Street" :: String, "City" :: String } { "Street" :: String, "City" :: String }
addressForm = Semigroupoid.do
  filledTextField @"Street"
  filledTextField @"City"
