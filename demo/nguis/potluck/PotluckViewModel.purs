module PotluckViewModel (guestCountLine, guestName, invitation, menuLine, menuState, waitingLine) where

import Prelude ((#), (<>), map, show)

import Data.Array (cons, foldr, length)
import Data.String (joinWith)
import Data.Variant (match)
import Data.Variant.Case (caseText)

invitation
  :: { "Guests" :: Array { "Dish" :: [ chosen :: [ "Lasagna" :: {}, "Pavlova" :: {}, "Salad" :: {} ]
                                     , unchosen :: {}
                                     ]
                         , name :: String
                         }
     }
invitation =
  { "Guests":
    [ { name: "Ada", "Dish": .unchosen {} }
    , { name: "Grace", "Dish": .unchosen {} }
    , { name: "Edsger", "Dish": .unchosen {} }
    , { name: "Barbara", "Dish": .unchosen {} }
    ]
  }

guestCountLine
  :: { "Guests" :: Array { "Dish" :: [ chosen :: [ "Lasagna" :: {}, "Pavlova" :: {}, "Salad" :: {} ]
                                     , unchosen :: {}
                                     ]
                         , name :: String
                         }
     }
  -> String
guestCountLine { "Guests": guests } = show (length guests) <> " guests invited — everyone picks one dish; the menu prints once the table is complete."

guestName
  :: { "Dish" :: [ chosen :: [ "Lasagna" :: {}, "Pavlova" :: {}, "Salad" :: {} ], unchosen :: {} ]
     , name :: String
     }
  -> String
guestName { name } = name

menuState
  :: { "Guests" :: Array { "Dish" :: [ chosen :: [ "Lasagna" :: {}, "Pavlova" :: {}, "Salad" :: {} ]
                                     , unchosen :: {}
                                     ]
                         , name :: String
                         }
     }
  -> [ complete :: {}, waiting :: {} ]
menuState invited = case table invited of
  { remaining: [] } -> .complete {}
  _ -> .waiting {}

menuLine
  :: { "Guests" :: Array { "Dish" :: [ chosen :: [ "Lasagna" :: {}, "Pavlova" :: {}, "Salad" :: {} ]
                                     , unchosen :: {}
                                     ]
                         , name :: String
                         }
     }
  -> String
menuLine invited = "On the table: " <> joinWith ", " (map (\d -> d.name <> "’s " <> caseText d.dish) (table invited).dishes)

waitingLine
  :: { "Guests" :: Array { "Dish" :: [ chosen :: [ "Lasagna" :: {}, "Pavlova" :: {}, "Salad" :: {} ]
                                     , unchosen :: {}
                                     ]
                         , name :: String
                         }
     }
  -> String
waitingLine invited = "Still choosing: " <> joinWith ", " (table invited).remaining

table
  :: { "Guests" :: Array { "Dish" :: [ chosen :: [ "Lasagna" :: {}, "Pavlova" :: {}, "Salad" :: {} ]
                                     , unchosen :: {}
                                     ]
                         , name :: String
                         }
     }
  -> { dishes :: Array { dish :: [ "Lasagna" :: {}, "Pavlova" :: {}, "Salad" :: {} ], name :: String }
     , remaining :: Array String
     }
table { "Guests": guests } = foldr sorted { dishes: [], remaining: [] } guests
  where
  sorted guest t = guest."Dish" # match
    { chosen: \dish -> t { dishes = cons { name: guest.name, dish } t.dishes }
    , unchosen: \_ -> t { remaining = cons guest.name t.remaining }
    }
