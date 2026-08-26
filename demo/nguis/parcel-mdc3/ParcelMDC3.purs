module ParcelMDC3 (parcelMDC3) where

import Prelude (Unit, (#), ($))

import Data.Profunctor.Row.RecordToRecord as RecordToRecord
import Effect (Effect)
import ParcelLogic (draftParcel)
import PUI (PUI, subStrong, mvu)
import PUI.Web.HTML (shown, body, staticText, text)
import PUI.Web.MDC3 (bodyLarge, card, elevation5, filledTextField)
import PUI.Web (Web)
import QualifiedDo.Semigroupoid as Pipeline

parcelMDC3 :: Effect Unit
parcelMDC3 =
  body $
    elevation5 $
      card $ ( Pipeline.do
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
addressForm = Pipeline.do
  filledTextField @"Street" {}
  filledTextField @"City" {}
