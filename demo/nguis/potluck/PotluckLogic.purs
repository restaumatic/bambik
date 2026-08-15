module PotluckLogic (dishText, guestCount, invitation) where

import Prelude (show)

import Data.Array (length)
import Data.Maybe (Maybe(..))
import Data.Variant (match)

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

dishText :: [ "Salad" :: {}, "Lasagna" :: {}, "Pavlova" :: {} ] -> String
dishText = match { "Salad": \_ -> "Salad", "Lasagna": \_ -> "Lasagna", "Pavlova": \_ -> "Pavlova" }
