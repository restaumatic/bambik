module Parcel (draftParcel, parcelLine) where

import Prelude ((<>))

draftParcel :: { "Address" :: { "City" :: String, "Street" :: String }, "Recipient" :: String }
draftParcel = { "Recipient": "Ada Lovelace", "Address": { "Street": "12 Analytical Row", "City": "London" } }

parcelLine
  :: { "Address" :: { "City" :: String, "Street" :: String }, "Recipient" :: String }
  -> String
parcelLine r = r."Recipient" <> " · " <> r."Address"."Street" <> " · " <> r."Address"."City"
