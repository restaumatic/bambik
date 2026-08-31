module PotluckLogic (invitation, menu) where

import Prelude (map, show)

import Data.Array (length)
import Data.Maybe (Maybe(..))
import Data.Variant.Case (caseText)

invitation :: { guests :: Array { name :: String, "Dish" :: Maybe [ "Salad" :: {}, "Lasagna" :: {}, "Pavlova" :: {} ] }, guestCountText :: String }
invitation = presentPotluck
  { guests:
      [ { name: "Ada", "Dish": Nothing }
      , { name: "Grace", "Dish": Nothing }
      , { name: "Edsger", "Dish": Nothing }
      , { name: "Barbara", "Dish": Nothing }
      ]
  , guestCountText: ""
  }

presentPotluck :: { guests :: Array { name :: String, "Dish" :: Maybe [ "Salad" :: {}, "Lasagna" :: {}, "Pavlova" :: {} ] }, guestCountText :: String } -> { guests :: Array { name :: String, "Dish" :: Maybe [ "Salad" :: {}, "Lasagna" :: {}, "Pavlova" :: {} ] }, guestCountText :: String }
presentPotluck r = r { guestCountText = show (length r.guests) }

menu :: Array { name :: String, "Dish" :: [ "Salad" :: {}, "Lasagna" :: {}, "Pavlova" :: {} ] } -> Array { name :: String, dish :: String }
menu = map \{ name, "Dish": dish } -> { name, dish: caseText dish }
