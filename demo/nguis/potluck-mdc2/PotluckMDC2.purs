module PotluckMDC2 (potluckMDC2) where

import Prelude ((#), ($), Unit)

import Data.Profunctor.Row.RecordToRecord as RecordToRecord
import Effect (Effect)
import PotluckLogic (guestCountLine, guestName, invitation, menuLine)
import PUI (acted, foreach, with)
import PUI.Web (choice)
import PUI.Web.HTML (shown, body, span, staticText, text)
import PUI.Web.MDC2 (body2, elevation20, group, headline6, list, listItem, segmentedButton, subtitle1)
import QualifiedDo.Category as Category

potluckMDC2 :: Effect Unit
potluckMDC2 =
  body $
    elevation20 $ ( Category.do
        body2 (text guestCountLine) # shown
        ( list $
            ( listItem $ RecordToRecord.do
                subtitle1 (text guestName)
                segmentedButton @"Dish"
                  [ choice @"Salad", choice @"Lasagna", choice @"Pavlova" ] ) # acted @"name" ) # group @"Guests"
        headline6 $ Category.do
          (staticText "On the table: ") # shown
          ( span $ text menuLine ) # foreach @"name" _."Guests"
    ) # with invitation
