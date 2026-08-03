module PotluckLogic (dishText, guestCount, invitation) where

import Prelude (show)

import Data.Array (length)
import Data.Maybe (Maybe(..))
import Data.Variant (match)

invitation :: { guests :: Array { name :: String, dish :: Maybe [ salad :: {}, lasagna :: {}, pavlova :: {} ] } }
invitation =
  { guests:
      [ { name: "Ada", dish: Nothing }
      , { name: "Grace", dish: Nothing }
      , { name: "Edsger", dish: Nothing }
      , { name: "Barbara", dish: Nothing }
      ]
  }

guestCount :: { guests :: Array { name :: String, dish :: Maybe [ salad :: {}, lasagna :: {}, pavlova :: {} ] } } -> String
guestCount { guests } = show (length guests)

dishText :: [ salad :: {}, lasagna :: {}, pavlova :: {} ] -> String
dishText = match { salad: \_ -> "Salad", lasagna: \_ -> "Lasagna", pavlova: \_ -> "Pavlova" }
