module ParcelViewModel (draftParcel, parcelLine) where

import Prelude ((<>))

draftParcel :: { "City" :: String, "Recipient" :: String, "Street" :: String }
draftParcel = { "Recipient": "Ada Lovelace", "Street": "12 Analytical Row", "City": "London" }

parcelLine :: { "City" :: String, "Recipient" :: String, "Street" :: String } -> String
parcelLine r = r."Recipient" <> " · " <> r."Street" <> " · " <> r."City"
