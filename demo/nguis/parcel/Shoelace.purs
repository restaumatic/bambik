module Parcel.Shoelace (parcel) where

import Prelude (Unit, (#), ($))

import Effect (Effect)
import Parcel (draftParcel, parcelLine)
import PUI (looped, PUI, with)
import PUI.Web (Web, text)
import PUI.Web.HTML (p)
import PUI.Web.Shoelace (body, group, textField)
import QualifiedDo.Semigroupoid as Semigroupoid

parcel :: Effect Unit
parcel =
  body $ Semigroupoid.do
    textField @"Recipient"
    group @"Address" addressForm
    p $ text parcelLine
  # looped # with draftParcel

addressForm
  :: PUI Web { "City" :: String, "Street" :: String } { "City" :: String, "Street" :: String }
addressForm = Semigroupoid.do
  textField @"Street"
  textField @"City"
