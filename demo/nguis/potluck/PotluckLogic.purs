module PotluckLogic (invitation, menu) where

import Prelude ((<>), map, show)

import Data.Array (length)
import Data.Maybe (Maybe(..))
import Data.Variant.Case (caseText)

invitation :: { guests :: Array { name :: String, "Dish" :: Maybe [ "Salad" :: {}, "Lasagna" :: {}, "Pavlova" :: {} ] }, guestCountLine :: String }
invitation = presentPotluck
  { guests:
      [ { name: "Ada", "Dish": Nothing }
      , { name: "Grace", "Dish": Nothing }
      , { name: "Edsger", "Dish": Nothing }
      , { name: "Barbara", "Dish": Nothing }
      ]
  , guestCountLine: ""
  }

presentPotluck :: { guests :: Array { name :: String, "Dish" :: Maybe [ "Salad" :: {}, "Lasagna" :: {}, "Pavlova" :: {} ] }, guestCountLine :: String } -> { guests :: Array { name :: String, "Dish" :: Maybe [ "Salad" :: {}, "Lasagna" :: {}, "Pavlova" :: {} ] }, guestCountLine :: String }
presentPotluck r = r { guestCountLine = show (length r.guests) <> " guests invited — everyone picks one dish; the menu prints once the table is complete." }

menu :: Array { name :: String, "Dish" :: [ "Salad" :: {}, "Lasagna" :: {}, "Pavlova" :: {} ] } -> Array { name :: String, menuLine :: String }
menu = map \{ name, "Dish": dish } -> { name, menuLine: name <> "’s " <> caseText dish <> ", " }
