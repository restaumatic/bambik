module PotluckMDC2 (potluckMDC2) where

import Prelude ((#), ($), Unit)

import Effect (Effect)
import PUI.Pipeline as Pipeline
import PotluckViewModel (guestCountLine, guestName, invitation, menuLine, menuState, waitingLine)
import PUI (acted, with)
import PUI.Web ((<+>), choice, pane, text)
import PUI.Web.MDC2 (body, body2, group, headline6, list, listItem, segmentedButtonUnpicked, subtitle1)

potluckMDC2 :: Effect Unit
potluckMDC2 =
  body $ Pipeline.do
    body2 (text guestCountLine)
    group @"Guests" $ list $
      ( listItem $ Pipeline.do
        subtitle1 (text guestName)
        segmentedButtonUnpicked @"Dish" @"chosen"
          (choice @"Salad" <+> choice @"Lasagna" <+> choice @"Pavlova") ) # acted @"name"
    headline6 (text menuLine) # pane @"complete"
      @( complete :: { dishes :: Array { name :: String, dish :: [ "Salad" :: {}, "Lasagna" :: {}, "Pavlova" :: {} ] } }
       , waiting :: { remaining :: Array String }
       ) menuState
    body2 (text waitingLine) # pane @"waiting" menuState
  # with
    @{ "Guests" :: Array { name :: String, "Dish" :: [ chosen :: [ "Salad" :: {}, "Lasagna" :: {}, "Pavlova" :: {} ], unchosen :: {} ] }
     }
    invitation
