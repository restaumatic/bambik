module PotluckMD3 (potluckMD3) where

import Prelude ((#), ($), Unit, show)

import Data.Array (length)
import Data.Maybe (Maybe(..))
import Data.Profunctor.Row.RecordToRecord as RecordToRecord
import Data.Variant (match)
import Effect (Effect)
import PUI (acted, asField, displayed, field, foreach, forField, forValue, projection, tapped, with)
import PUI.HTML (body, span, staticText, text)
import PUI.MDC3 (bodyMedium, card, elevation5, headlineSmall, list, listItem, segmentedButton, titleMedium)
import QualifiedDo.Semigroupoid as Semigroupoid

potluckMD3 :: Effect Unit
potluckMD3 =
  body $
    elevation5 $
      card { caption: "Potluck" } $ ( Semigroupoid.do
          bodyMedium ( Semigroupoid.do
              text # projection guestCount
              staticText " guests invited — everyone picks one dish; the menu prints once the table is complete." # displayed
          ) # tapped
          ( list $
              ( listItem $ RecordToRecord.do
                  titleMedium text # forValue # forField @"name"
                  segmentedButton
                    [ { value: .salad {}, label: "Salad" }
                    , { value: .lasagna {}, label: "Lasagna" }
                    , { value: .pavlova {}, label: "Pavlova" }
                    ] # asField @"dish"
              ) # acted @"name" ) # field @"guests"
          headlineSmall $ Semigroupoid.do
            staticText "On the table: " # displayed
            ( span $ RecordToRecord.do
                text # forValue # forField @"name"
                staticText "’s "
                text # projection dishText # forField @"dish"
                staticText ", "
            ) # foreach @"name" # field @"guests"
      ) # with invitation

guestCount :: { guests :: Array { name :: String, dish :: Maybe [ salad :: {}, lasagna :: {}, pavlova :: {} ] } } -> String
guestCount { guests } = show (length guests)

dishText :: [ salad :: {}, lasagna :: {}, pavlova :: {} ] -> String
dishText = match { salad: \_ -> "Salad", lasagna: \_ -> "Lasagna", pavlova: \_ -> "Pavlova" }

invitation :: { guests :: Array { name :: String, dish :: Maybe [ salad :: {}, lasagna :: {}, pavlova :: {} ] } }
invitation =
  { guests:
      [ { name: "Ada", dish: Nothing }
      , { name: "Grace", dish: Nothing }
      , { name: "Edsger", dish: Nothing }
      , { name: "Barbara", dish: Nothing }
      ]
  }
