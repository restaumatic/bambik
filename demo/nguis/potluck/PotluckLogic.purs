module PotluckLogic (guestCountLine, guestName, invitation, menuLine, menuState, waitingLine) where

import Prelude ((#), (<>), map, show)

import Data.Array (cons, foldr, length)
import Data.String (joinWith)
import Data.Variant (match)
import Data.Variant.Case (caseText)

invitation :: { "Guests" :: Array { name :: String, "Dish" :: [ chosen :: [ "Salad" :: {}, "Lasagna" :: {}, "Pavlova" :: {} ], unchosen :: {} ] } }
invitation =
  { "Guests":
    [ { name: "Ada", "Dish": .unchosen {} }
    , { name: "Grace", "Dish": .unchosen {} }
    , { name: "Edsger", "Dish": .unchosen {} }
    , { name: "Barbara", "Dish": .unchosen {} }
    ]
  }

guestCountLine :: { "Guests" :: Array { name :: String, "Dish" :: [ chosen :: [ "Salad" :: {}, "Lasagna" :: {}, "Pavlova" :: {} ], unchosen :: {} ] } } -> String
guestCountLine { "Guests": guests } = show (length guests) <> " guests invited — everyone picks one dish; the menu prints once the table is complete."

guestName :: { name :: String } -> String
guestName { name } = name

menuState :: { "Guests" :: Array { name :: String, "Dish" :: [ chosen :: [ "Salad" :: {}, "Lasagna" :: {}, "Pavlova" :: {} ], unchosen :: {} ] } } -> [ complete :: { dishes :: Array { name :: String, dish :: [ "Salad" :: {}, "Lasagna" :: {}, "Pavlova" :: {} ] } }, waiting :: { remaining :: Array String } ]
menuState { "Guests": guests } = case foldr sorted { dishes: [], remaining: [] } guests of
  { dishes, remaining: [] } -> .complete { dishes }
  { remaining } -> .waiting { remaining }
  where
  sorted guest table = guest."Dish" # match
    { chosen: \dish -> table { dishes = cons { name: guest.name, dish } table.dishes }
    , unchosen: \_ -> table { remaining = cons guest.name table.remaining }
    }

menuLine :: { dishes :: Array { name :: String, dish :: [ "Salad" :: {}, "Lasagna" :: {}, "Pavlova" :: {} ] } } -> String
menuLine { dishes } = "On the table: " <> joinWith ", " (map (\d -> d.name <> "’s " <> caseText d.dish) dishes)

waitingLine :: { remaining :: Array String } -> String
waitingLine { remaining } = "Still choosing: " <> joinWith ", " remaining
