module ParcelMDC2 (parcelMDC2) where

import Prelude (Unit, (#), ($))

import Effect (Effect)
import ParcelLogic (draftParcel, parcelLine)
import PUI (PUI, subStrong, mvu)
import PUI.Web.MDC2 (body, body1, card, filledTextField)
import PUI.Web (Web)
import PUI.Web.HTML (shown, text)
import QualifiedDo.Category as Category

parcelMDC2 :: Effect Unit
parcelMDC2 =
  body $
    card $ ( Category.do
      filledTextField @"Recipient" {}
      addressForm # subStrong
      ( body1 $ text parcelLine ) # shown
    ) # mvu draftParcel

addressForm :: PUI Web { "Street" :: String, "City" :: String } { "Street" :: String, "City" :: String }
addressForm = Category.do
  filledTextField @"Street" {}
  filledTextField @"City" {}
