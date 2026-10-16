module Parcel (Parcel, draftParcel, parcelLine) where

import Prelude ((<>))

type Parcel = { "Address" :: { "City" :: String, "Street" :: String }, "Recipient" :: String }

draftParcel :: Parcel
draftParcel = { "Recipient": "Ada Lovelace", "Address": { "Street": "12 Analytical Row", "City": "London" } }

parcelLine :: Parcel -> String
parcelLine r = r."Recipient" <> " · " <> r."Address"."Street" <> " · " <> r."Address"."City"
