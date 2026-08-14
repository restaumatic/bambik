module PotluckMDC2 (potluckMDC2) where

import Prelude (identity, (#), ($), Unit)

import Data.Profunctor.Row.RecordToRecord as RecordToRecord
import Effect (Effect)
import PotluckLogic (dishText, guestCount, invitation)
import PUI (acted, displayed, field, foreach, projection, projected, tapped, with)
import PUI.Web.HTML (body, span, staticText, text)
import PUI.Web.MDC2 (body2, card, elevation20, headline6, list, listItem, segmentedButton, subtitle1)
import QualifiedDo.Semigroupoid as Semigroupoid

potluckMDC2 :: Effect Unit
potluckMDC2 =
  body $
    elevation20 $
      card { caption: "Potluck" } $ ( Semigroupoid.do
          body2 ( Semigroupoid.do
              text @"guestCount" # projected guestCount
              staticText " guests invited — everyone picks one dish; the menu prints once the table is complete." # displayed ) # tapped
          ( list $
              ( listItem $ RecordToRecord.do
                  subtitle1 (text @"name")
                  segmentedButton @"Dish"
                    [ { value: .salad {}, label: "Salad" }
                    , { value: .lasagna {}, label: "Lasagna" }
                    , { value: .pavlova {}, label: "Pavlova" }
                    ] ) # acted @"name" ) # field @"guests"
          headline6 $ Semigroupoid.do
            staticText "On the table: " # displayed
            ( span $ RecordToRecord.do
                text @"name"
                staticText "’s "
                text @"Dish" # projection dishText
                staticText ", " ) # foreach @"name" identity # field @"guests"
      ) # with invitation
