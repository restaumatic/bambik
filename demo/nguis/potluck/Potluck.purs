module Potluck (Dish, Guest, Invitation, guestCountLine, invitation, menuLine, menuState, waitingLine) where

import Prelude ((#), (<>), map, show)

import Data.Array (cons, foldr, length)
import Data.String (joinWith)
import Data.Variant (match)
import Data.Variant.Case (caseText)

type Dish = [ "Lasagna" :: {}, "Pavlova" :: {}, "Salad" :: {} ]

type Guest = { "Dish" :: [ chosen :: Dish, unchosen :: {} ], name :: String }

type Invitation = { "Guests" :: Array Guest }

invitation :: Invitation
invitation =
  { "Guests":
    [ { name: "Ada", "Dish": .unchosen {} }
    , { name: "Grace", "Dish": .unchosen {} }
    , { name: "Edsger", "Dish": .unchosen {} }
    , { name: "Barbara", "Dish": .unchosen {} }
    ]
  }

guestCountLine :: Invitation -> String
guestCountLine { "Guests": guests } = show (length guests) <> " guests invited — everyone picks one dish; the menu prints once the table is complete."

menuState :: Invitation -> [ complete :: {}, waiting :: {} ]
menuState invited = case table invited of
  { remaining: [] } -> .complete {}
  _ -> .waiting {}

menuLine :: Invitation -> String
menuLine invited = "On the table: " <> joinWith ", " (map (\d -> d.name <> "’s " <> caseText d.dish) (table invited).dishes)

waitingLine :: Invitation -> String
waitingLine invited = "Still choosing: " <> joinWith ", " (table invited).remaining

table
  :: Invitation
  -> { dishes :: Array { dish :: Dish, name :: String }, remaining :: Array String }
table { "Guests": guests } = foldr sorted { dishes: [], remaining: [] } guests
  where
  sorted guest t = guest."Dish" # match
    { chosen: \dish -> t { dishes = cons { name: guest.name, dish } t.dishes }
    , unchosen: \_ -> t { remaining = cons guest.name t.remaining }
    }
