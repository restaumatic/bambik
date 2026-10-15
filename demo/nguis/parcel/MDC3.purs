module Parcel.MDC3 (parcel) where

import Prelude (Unit, (#), ($))

import Effect (Effect)
import Parcel (draftParcel, parcelLine)
import PUI (looped, PUI, with)
import PUI.Web.MDC3 (body, bodyLarge, filledTextField, group)
import PUI.Web (Web, text)
import QualifiedDo.Semigroupoid as Semigroupoid

parcel :: Effect Unit
parcel =
  body $ Semigroupoid.do
    filledTextField @"Recipient"
    group @"Address" addressForm
    bodyLarge $ text parcelLine
  # looped # with draftParcel

addressForm :: PUI Web { "Street" :: String, "City" :: String } { "Street" :: String, "City" :: String }
addressForm = Semigroupoid.do
  filledTextField @"Street"
  filledTextField @"City"
