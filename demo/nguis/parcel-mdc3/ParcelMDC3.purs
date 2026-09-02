module ParcelMDC3 (parcelMDC3) where

import Prelude (Unit, (#), ($))

import Effect (Effect)
import ParcelLogic (draftParcel, presentParcel)
import PUI (PUI, subStrong, mvu, settled)
import PUI.Web.HTML (shown, body, text)
import PUI.Web.MDC3 (bodyLarge, card, elevation5, filledTextField)
import PUI.Web (Web)
import QualifiedDo.Category as Category

parcelMDC3 :: Effect Unit
parcelMDC3 =
  body $
    elevation5 $
      card $ ( Category.do
          filledTextField @"Recipient" {}
          addressForm # subStrong
          ( bodyLarge $ text @"parcelLine" ) # shown
      ) # settled presentParcel # mvu draftParcel

addressForm :: PUI Web { "Street" :: String, "City" :: String } { "Street" :: String, "City" :: String }
addressForm = Category.do
  filledTextField @"Street" {}
  filledTextField @"City" {}
