module PotluckLogic (guestCountLine, guestName, invitation, menuLine) where

import Prelude ((<>), map, show)

import Data.Array (length)
import Data.Maybe (Maybe(..))
import Data.String (joinWith)
import Data.Variant.Case (caseText)

invitation :: { "Guests" :: Array { name :: String, "Dish" :: Maybe [ "Salad" :: {}, "Lasagna" :: {}, "Pavlova" :: {} ] } }
invitation =
  { "Guests":
      [ { name: "Ada", "Dish": Nothing }
      , { name: "Grace", "Dish": Nothing }
      , { name: "Edsger", "Dish": Nothing }
      , { name: "Barbara", "Dish": Nothing }
      ]
  }

guestCountLine :: { "Guests" :: Array { name :: String, "Dish" :: Maybe [ "Salad" :: {}, "Lasagna" :: {}, "Pavlova" :: {} ] } } -> String
guestCountLine { "Guests": guests } = show (length guests) <> " guests invited — everyone picks one dish; the menu prints once the table is complete."

guestName :: { name :: String } -> String
guestName { name } = name

menuLine :: { "Guests" :: Array { name :: String, "Dish" :: [ "Salad" :: {}, "Lasagna" :: {}, "Pavlova" :: {} ] } } -> String
menuLine { "Guests": guests } = "On the table: " <> joinWith ", " (map (\g -> g.name <> "’s " <> caseText g."Dish") guests)
