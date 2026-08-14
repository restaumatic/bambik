module PotluckLogic (dishText, guestCount, invitation) where

import Prelude (show)

import Data.Array (length)
import Data.Maybe (Maybe(..))
import Data.Variant (match)

invitation :: { guests :: Array { name :: String, "Dish" :: Maybe [ salad :: {}, lasagna :: {}, pavlova :: {} ] } }
invitation =
  { guests:
      [ { name: "Ada", "Dish": Nothing }
      , { name: "Grace", "Dish": Nothing }
      , { name: "Edsger", "Dish": Nothing }
      , { name: "Barbara", "Dish": Nothing }
      ]
  }

guestCount :: { guests :: Array { name :: String, "Dish" :: Maybe [ salad :: {}, lasagna :: {}, pavlova :: {} ] } } -> String
guestCount { guests } = show (length guests)

dishText :: [ salad :: {}, lasagna :: {}, pavlova :: {} ] -> String
dishText = match { salad: \_ -> "Salad", lasagna: \_ -> "Lasagna", pavlova: \_ -> "Pavlova" }
