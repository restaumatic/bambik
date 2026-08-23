module ParcelLogic (addressLine, draftParcel) where

import Prelude ((<>))

draftParcel :: { "Recipient" :: String, "Street" :: String, "City" :: String }
draftParcel = { "Recipient": "Ada Lovelace", "Street": "12 Analytical Row", "City": "London" }

addressLine :: { "Recipient" :: String, "Street" :: String, "City" :: String } -> String
addressLine { "Recipient": recipient, "Street": street, "City": city } = recipient <> " \x00b7 " <> street <> " \x00b7 " <> city
