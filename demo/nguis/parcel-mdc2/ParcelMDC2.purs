module ParcelMDC2 (parcelMDC2) where

import Prelude (Unit, (#), ($))

import Data.Profunctor.Row.RecordToRecord as RecordToRecord
import Effect (Effect)
import ParcelLogic (draftParcel)
import PUI (PUI, subStrong, mvu)
import PUI.Web.HTML (shown, body, staticText, text)
import PUI.Web.MDC2 (body1, card, elevation20, filledTextField)
import PUI.Web (Web)
import QualifiedDo.Semigroupoid as Pipeline

parcelMDC2 :: Effect Unit
parcelMDC2 =
  body $
    elevation20 $
      card $ ( Pipeline.do
          filledTextField @"Recipient" {}
          addressForm # subStrong
          ( body1 $ RecordToRecord.do
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
