module Potluck (potluck) where

import Prelude ((#), ($), (<<<), Unit, show)

import Data.Array (length)
import Data.Maybe (Maybe(..))
import Data.Profunctor.Row.RecordToRecord as RecordToRecord
import Effect (Effect)
import PUI (acted, asField, displayed, foreach, forField, forValue, muted, projection, tapped, widenRecordInput, with)
import PUI.HTML (body, span, staticText, text)
import PUI.MDC (body2, card, elevation20, headline6, labeled, list, listItem, segmentedButton, subtitle1)
import QualifiedDo.Semigroupoid as Semigroupoid

potluck :: Effect Unit
potluck =
  body $
    elevation20 $
      card { caption: "Potluck" } $ ( Semigroupoid.do
          body2 ( Semigroupoid.do
              text # projection (show <<< length)
              staticText " guests invited — everyone picks one dish; the menu prints once the table is complete." # muted # displayed
          ) # tapped
          list $
            ( listItem $ Semigroupoid.do
                subtitle1 text # projection _.name # displayed
                segmentedButton (labeled dishes) # asField @"dish" # widenRecordInput
            ) # acted @"name"
          headline6 $ Semigroupoid.do
            staticText "On the table: " # muted # displayed
            ( span $ RecordToRecord.do
                text # forValue # forField @"name"
                staticText "’s "
                text # forValue # forField @"dish"
                staticText ", "
            ) # foreach @"name"
      ) # with invited

dishes :: Array String
dishes = [ "Salad", "Lasagna", "Pavlova" ]

invited :: Array { name :: String, dish :: Maybe String }
invited =
  [ { name: "Ada", dish: Nothing }
  , { name: "Grace", dish: Nothing }
  , { name: "Edsger", dish: Nothing }
  , { name: "Barbara", dish: Nothing }
  ]
