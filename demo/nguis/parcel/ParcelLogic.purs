module ParcelLogic (draftParcel, presentParcel) where

import Prelude ((<>))

draftParcel :: { "Recipient" :: String, "Street" :: String, "City" :: String, parcelLine :: String }
draftParcel = presentParcel { "Recipient": "Ada Lovelace", "Street": "12 Analytical Row", "City": "London", parcelLine: "" }

presentParcel :: { "Recipient" :: String, "Street" :: String, "City" :: String, parcelLine :: String } -> { "Recipient" :: String, "Street" :: String, "City" :: String, parcelLine :: String }
presentParcel r = r { parcelLine = r."Recipient" <> " · " <> r."Street" <> " · " <> r."City" }
