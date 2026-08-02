module ParcelMDC2 (parcelMDC2) where

import Prelude ((#), ($), (<>), Unit)

import Data.Profunctor.Row.RecordToRecord as RecordToRecord
import Effect (Effect)
import PUI (PUI, asField, completed, focusRecord, mvu, projected, tapped)
import PUI.HTML (body, text)
import PUI.MDC2 (body1, card, elevation20, filledTextField)
import PUI.Web (Web)
import QualifiedDo.Semigroupoid as Semigroupoid

parcelMDC2 :: Effect Unit
parcelMDC2 =
  body $
    elevation20 $
      card { caption: "Parcel" } $ ( Semigroupoid.do
          filledTextField { floatingLabel: "Recipient" } # asField @"recipient" # completed
          addressForm # focusRecord
          body1 text # projected labelLine # tapped
      ) # mvu draftParcel

addressForm :: PUI Web { street :: String, city :: String } { street :: String, city :: String }
addressForm = RecordToRecord.do
  filledTextField { floatingLabel: "Street" } # asField @"street"
  filledTextField { floatingLabel: "City" } # asField @"city"

labelLine :: { recipient :: String, street :: String, city :: String } -> String
labelLine { recipient, street, city } = recipient <> " · " <> street <> " · " <> city

draftParcel :: { recipient :: String, street :: String, city :: String }
draftParcel = { recipient: "Ada Lovelace", street: "12 Analytical Row", city: "London" }
