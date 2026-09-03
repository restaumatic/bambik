module ParcelLogic (draftParcel, parcelLine) where

import Prelude ((<>))

draftParcel :: { "Recipient" :: String, "Street" :: String, "City" :: String }
draftParcel = { "Recipient": "Ada Lovelace", "Street": "12 Analytical Row", "City": "London" }

parcelLine :: { "Recipient" :: String, "Street" :: String, "City" :: String } -> String
parcelLine r = r."Recipient" <> " · " <> r."Street" <> " · " <> r."City"
