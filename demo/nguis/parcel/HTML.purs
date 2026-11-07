module Parcel.HTML (parcel) where

import Prelude (Unit, (#), ($))

import Effect (Effect)
import Parcel (draftParcel, parcelLine)
import PUI (looped, PUI, with)
import PUI.Web (Web, staticText, text)
import PUI.Web.HTML (body, div, group, input, label, p)
import QualifiedDo.Semigroupoid as Semigroupoid

parcel :: Effect Unit
parcel =
  body $ div $ Semigroupoid.do
    p $ label $ Semigroupoid.do
      staticText "Recipient "
      input @"Recipient" "text"
    group @"Address" addressForm
    p $ text parcelLine
  # looped # with draftParcel

addressForm
  :: PUI Web { "City" :: String, "Street" :: String } { "City" :: String, "Street" :: String }
addressForm = Semigroupoid.do
  p $ label $ Semigroupoid.do
    staticText "Street "
    input @"Street" "text"
  p $ label $ Semigroupoid.do
    staticText "City "
    input @"City" "text"
