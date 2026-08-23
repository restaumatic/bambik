module PotluckLogic (guestCount, guestLine, invitation, invitationLine, nameText) where

import Data.Variant.Case (caseText)
import Prelude ((<>), show)

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

invitationLine :: { guests :: Array { name :: String, "Dish" :: Maybe [ "Salad" :: {}, "Lasagna" :: {}, "Pavlova" :: {} ] } } -> String
invitationLine party = guestCount party <> " guests invited \x2014 everyone picks one dish; the menu prints once the table is complete."

nameText :: { name :: String, "Dish" :: Maybe [ "Salad" :: {}, "Lasagna" :: {}, "Pavlova" :: {} ] } -> String
nameText { name } = name

guestLine :: { name :: String, "Dish" :: [ "Salad" :: {}, "Lasagna" :: {}, "Pavlova" :: {} ] } -> String
guestLine { name, "Dish": dish } = name <> "\x2019s " <> caseText dish <> ", "
