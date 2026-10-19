module Potluck.MDC2 (potluck) where

import Prelude ((#), ($), Unit)

import Data.Profunctor.Row.VariantToVariant as VariantToVariant
import Effect (Effect)
import Potluck (guestCountLine, invitation, menuLine, menuState, waitingLine)
import PUI (acted, with)
import PUI.Web (inCase, text, textAt)
import PUI.Web.MDC2 (body, body2, group, headline6, list, listItem, segment, segmentedButtonUnpicked, subtitle1)
import QualifiedDo.Semigroupoid as Semigroupoid

potluck :: Effect Unit
potluck =
  body $ Semigroupoid.do
    body2 $ text guestCountLine
    group @"Guests" $ list $
      ( listItem $ Semigroupoid.do
        subtitle1 $ textAt @"name"
        segmentedButtonUnpicked @"Dish" @"chosen" $ VariantToVariant.do
          segment @"Salad"
          segment @"Lasagna"
          segment @"Pavlova" ) # acted @"name"
    ( headline6 $ text menuLine ) # inCase @"complete" menuState
    ( body2 $ text waitingLine ) # inCase @"waiting" menuState
  # with invitation
