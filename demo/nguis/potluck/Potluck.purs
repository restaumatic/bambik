module Potluck (potluck) where

import Prelude ((#), ($), (<>), Unit, show)

import Data.Array (length, zipWith)
import Data.Maybe (Maybe(..))
import Data.Profunctor (lcmap)
import Data.Profunctor.Acting (acted)
import Data.String (joinWith)
import Effect (Effect)
import PUI (asField, displayed, projection, tapped, with)
import PUI.HTML (body, text)
import PUI.MDC (body2, card, elevation20, headline6, list, listItem, segmentedButton, subtitle1)
import QualifiedDo.Semigroupoid as Semigroupoid

type Guest = { name :: String, dish :: Maybe String }

potluck :: Effect Unit
potluck =
  body $
    elevation20 $
      card { caption: "Potluck" } $ ( Semigroupoid.do
          body2 text # projection callToAction # tapped
          list $
            ( listItem $ Semigroupoid.do
                subtitle1 text # projection _.name # displayed
                segmentedButton dishes # asField @"dish" # lcmap pickOf
            ) # acted _.name
          headline6 text # projection menu
      ) # with invited

dishes :: Array { value :: String, label :: String }
dishes =
  [ { value: "Salad", label: "Salad" }
  , { value: "Lasagna", label: "Lasagna" }
  , { value: "Pavlova", label: "Pavlova" }
  ]

invited :: Array Guest
invited =
  [ { name: "Ada", dish: Nothing }
  , { name: "Grace", dish: Nothing }
  , { name: "Edsger", dish: Nothing }
  , { name: "Barbara", dish: Nothing }
  ]

pickOf :: Guest -> { dish :: Maybe String }
pickOf guest = { dish: guest.dish }

callToAction :: Array Guest -> String
callToAction guests = show (length guests) <> " guests invited — everyone picks one dish; the menu prints once the table is complete."

menu :: Array { dish :: String } -> String
menu picks = "On the table: " <> joinWith ", " (zipWith (\g p -> g.name <> "’s " <> p.dish) invited picks)
