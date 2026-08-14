module PotluckMDC3 (potluckMDC3) where

import Prelude (identity, (#), ($), Unit)

import Data.Profunctor.Row.RecordToRecord as RecordToRecord
import Effect (Effect)
import PotluckLogic (dishText, guestCount, invitation)
import PUI (acted, displayed, field, foreach, projection, projected, tapped, with)
import PUI.Web.HTML (body, span, staticText, text)
import PUI.Web.MDC3 (bodyMedium, card, elevation5, headlineSmall, list, listItem, segmentedButton, titleMedium)
import QualifiedDo.Semigroupoid as Semigroupoid

potluckMDC3 :: Effect Unit
potluckMDC3 =
  body $
    elevation5 $
      card { caption: "Potluck" } $ ( Semigroupoid.do
          bodyMedium ( Semigroupoid.do
              text @"guestCount" # projected guestCount
              staticText " guests invited — everyone picks one dish; the menu prints once the table is complete." # displayed ) # tapped
          ( list $
              ( listItem $ RecordToRecord.do
                  titleMedium (text @"name")
                  segmentedButton @"dish"
                    [ { value: .salad {}, label: "Salad" }
                    , { value: .lasagna {}, label: "Lasagna" }
                    , { value: .pavlova {}, label: "Pavlova" }
                    ] ) # acted @"name" ) # field @"guests"
          headlineSmall $ Semigroupoid.do
            staticText "On the table: " # displayed
            ( span $ RecordToRecord.do
                text @"name"
                staticText "’s "
                text @"dish" # projection dishText
                staticText ", " ) # foreach @"name" identity # field @"guests"
      ) # with invitation
