module PotluckLogic (guestCount, invitation) where

import Prelude (show)

import Data.Array (length)
import Data.Maybe (Maybe(..))

invitation :: { guests :: Array { name :: String, "Dish" :: Maybe [ "Salad" :: {}, "Lasagna" :: {}, "Pavlova" :: {} ] } }
invitation =
  { guests:
      [ { name: "Ada", "Dish": Nothing }
      , { name: "Grace", "Dish": Nothing }
      , { name: "Edsger", "Dish": Nothing }
      , { name: "Barbara", "Dish": Nothing }
      ]
  }

guestCount :: { guests :: Array { name :: String, "Dish" :: Maybe [ "Salad" :: {}, "Lasagna" :: {}, "Pavlova" :: {} ] } } -> String
guestCount { guests } = show (length guests)
