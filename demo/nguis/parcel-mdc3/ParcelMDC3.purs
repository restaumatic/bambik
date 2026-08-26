module ParcelMDC3 (parcelMDC3) where

import Prelude (Unit, (#), ($))

import Data.Profunctor.Row.RecordToRecord as RecordToRecord
import Effect (Effect)
import ParcelLogic (draftParcel)
import PUI (PUI, subStrong, mvu)
import PUI.Web.HTML (shown, body, staticText, text)
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
          ( bodyLarge $ RecordToRecord.do
              text @"Recipient"
              staticText " · "
              text @"Street"
              staticText " · "
              text @"City" ) # shown
      ) # mvu draftParcel

addressForm :: PUI Web { "Street" :: String, "City" :: String } { "Street" :: String, "City" :: String }
addressForm = Category.do
  filledTextField @"Street" {}
  filledTextField @"City" {}
