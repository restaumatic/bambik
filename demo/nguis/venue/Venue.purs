module Venue (Day, Venue, Zone, addressLine, dayLine, deliveryLine, signLine, tradingDays, trattoria, zoneLine) where

import Prelude ((<>), (==), map, show)

import Data.Array (filter, length)
import Data.Foldable (minimum)
import Data.Maybe (Maybe(..))
import Data.Number.Format (fixed, toStringWith)
import Data.Variant (match)
import Data.Variant.Case (caseText)

type Venue =
  { "Address" :: { "City" :: String, "Street" :: String }
  , "Cuisine" :: [ "Italian" :: {}, "Japanese" :: {}, "Mexican" :: {} ]
  , "Delivery" ::
      { "Partner name" :: String
      , "Service" :: [ "None" :: {}, "Own couriers" :: {}, "Partner" :: {} ]
      , zones :: Array Zone
      }
  , "Name" :: String
  , "Opening hours" :: Array Day
  }

type Day = { "Closes" :: String, "Opens" :: String, "Trading" :: [ "Closed" :: {}, "Open" :: {} ], day :: String }

type Zone =
  { "Fee" :: { current :: Number, max :: Number, min :: Number, step :: [ continuous :: {}, discrete :: Number ] }
  , zone :: String
  }

trattoria :: Venue
trattoria =
  { "Name": "Trattoria Yoneda"
  , "Cuisine": ."Italian" {}
  , "Address": { "Street": "Via Lemma 4", "City": "Bologna" }
  , "Opening hours":
    [ { day: "Monday", "Trading": ."Closed" {}, "Opens": "12:00", "Closes": "22:00" }
    , { day: "Tuesday", "Trading": ."Open" {}, "Opens": "12:00", "Closes": "22:00" }
    , { day: "Wednesday", "Trading": ."Open" {}, "Opens": "12:00", "Closes": "22:00" }
    , { day: "Thursday", "Trading": ."Open" {}, "Opens": "12:00", "Closes": "22:00" }
    , { day: "Friday", "Trading": ."Open" {}, "Opens": "12:00", "Closes": "23:30" }
    , { day: "Saturday", "Trading": ."Open" {}, "Opens": "10:00", "Closes": "23:30" }
    , { day: "Sunday", "Trading": ."Open" {}, "Opens": "10:00", "Closes": "16:00" }
    ]
  , "Delivery":
    { "Service": ."Own couriers" {}
    , "Partner name": ""
    , zones:
      [ { zone: "Old Town", "Fee": { current: 2.5, min: 0.0, max: 10.0, step: .discrete 0.5 } }
      , { zone: "Riverside", "Fee": { current: 3.5, min: 0.0, max: 10.0, step: .discrete 0.5 } }
      , { zone: "The Hills", "Fee": { current: 5.0, min: 0.0, max: 10.0, step: .discrete 0.5 } }
      ]
    }
  }

signLine :: Venue -> String
signLine venue = venue."Name" <> " · " <> caseText venue."Cuisine"

addressLine :: Venue -> String
addressLine { "Address": address } = address."Street" <> ", " <> address."City"

tradingDays :: Venue -> Array Day
tradingDays { "Opening hours": days } = filter (\d -> d."Trading" == ."Open" {}) days

dayLine :: Day -> String
dayLine d = d.day <> " " <> d."Opens" <> "–" <> d."Closes"

zoneLine :: Zone -> String
zoneLine { zone, "Fee": fee } = zone <> " · €" <> toStringWith (fixed 2) fee.current

deliveryLine :: Venue -> String
deliveryLine { "Delivery": delivery } = match
  { "None": \_ -> "Collection only"
  , "Partner": \_ -> case delivery."Partner name" of
    "" -> "Delivered by a partner"
    partner -> "Delivered by " <> partner
  , "Own couriers": \_ -> case minimum (map _."Fee".current delivery.zones) of
    Just lowestFee -> "Delivers to " <> zonesText <> " from €" <> toStringWith (fixed 2) lowestFee
    Nothing -> "Delivers itself, no zone set yet"
  } delivery."Service"
  where
  zonesText = case length delivery.zones of
    1 -> "1 zone"
    n -> show n <> " zones"
