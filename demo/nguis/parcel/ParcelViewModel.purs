module ParcelViewModel (draftParcel, parcelLine) where

import Prelude ((<>))

draftParcel :: { "Recipient" :: String, "Street" :: String, "City" :: String }
draftParcel = { "Recipient": "Ada Lovelace", "Street": "12 Analytical Row", "City": "London" }

parcelLine :: forall r1. { "Recipient" :: String, "Street" :: String, "City" :: String | r1 } -> String
parcelLine r = r."Recipient" <> " · " <> r."Street" <> " · " <> r."City"
