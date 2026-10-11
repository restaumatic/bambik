module PotluckMDC3 (potluckMDC3) where

import Prelude ((#), ($), Unit)

import Effect (Effect)
import PUI.Pipeline as Pipeline
import PotluckViewModel (guestCountLine, guestName, invitation, menuLine, menuState, waitingLine)
import PUI (acted, with)
import PUI.Web ((<+>), choice, pane, text)
import PUI.Web.MDC3 (body, bodyMedium, group, headlineSmall, list, listItem, segmentedButtonUnpicked, titleMedium)

potluckMDC3 :: Effect Unit
potluckMDC3 =
  body $ Pipeline.do
    bodyMedium (text guestCountLine)
    group @"Guests" $ list $
      ( listItem $ Pipeline.do
        titleMedium (text guestName)
        segmentedButtonUnpicked @"Dish" @"chosen"
          (choice @"Salad" <+> choice @"Lasagna" <+> choice @"Pavlova") ) # acted @"name"
    headlineSmall (text menuLine) # pane @"complete"
      @( complete :: { dishes :: Array { name :: String, dish :: [ "Salad" :: {}, "Lasagna" :: {}, "Pavlova" :: {} ] } }
       , waiting :: { remaining :: Array String }
       ) menuState
    bodyMedium (text waitingLine) # pane @"waiting" menuState
  # with
    @{ "Guests" :: Array { name :: String, "Dish" :: [ chosen :: [ "Salad" :: {}, "Lasagna" :: {}, "Pavlova" :: {} ], unchosen :: {} ] }
     }
    invitation
