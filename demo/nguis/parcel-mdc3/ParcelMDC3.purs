module ParcelMDC3 (parcelMDC3) where

import Prelude (Unit, (#), ($))

import Effect (Effect)
import ParcelLogic (draftParcel, parcelLine)
import PUI (PUI, subStrong, mvu)
import PUI.Web.MDC3 (body, bodyLarge, card, filledTextField)
import PUI.Web (Web)
import PUI.Web.HTML (shown, text)
import QualifiedDo.Category as Category

parcelMDC3 :: Effect Unit
parcelMDC3 =
  body $
    card $ ( Category.do
        filledTextField @"Recipient" {}
        addressForm # subStrong
        ( bodyLarge $ text parcelLine ) # shown
    ) # mvu draftParcel

addressForm :: PUI Web { "Street" :: String, "City" :: String } { "Street" :: String, "City" :: String }
addressForm = Category.do
  filledTextField @"Street" {}
  filledTextField @"City" {}
