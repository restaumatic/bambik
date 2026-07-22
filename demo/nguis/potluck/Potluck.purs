module Potluck (potluck) where

import Prelude ((#), ($), (<>), (==), Unit, show)

import Data.Array (length, mapWithIndex)
import Data.Maybe (Maybe(..))
import Data.Profunctor (lcmap)
import Data.Profunctor.Acting (acted)
import Effect (Effect)
import PUI (asField, displayed, focusRecord, muted, projection, tapped, with)
import PUI.HTML (body, foreach, staticText, text)
import PUI.MDC (body2, card, elevation20, headline6, labeled, list, listItem, segmentedButton, subtitle1)
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
                segmentedButton (labeled dishes) # asField @"dish" # focusRecord
            ) # acted _.name
          headline6 $ Semigroupoid.do
            displayed (muted (staticText "On the table: "))
            (text # lcmap servingText) # foreach _.name # lcmap menuLines
      ) # with invited

dishes :: Array String
dishes = [ "Salad", "Lasagna", "Pavlova" ]

invited :: Array Guest
invited =
  [ { name: "Ada", dish: Nothing }
  , { name: "Grace", dish: Nothing }
  , { name: "Edsger", dish: Nothing }
  , { name: "Barbara", dish: Nothing }
  ]

callToAction :: Array Guest -> String
callToAction guests = show (length guests) <> " guests invited — everyone picks one dish; the menu prints once the table is complete."

menuLines :: Array { name :: String, dish :: String } -> Array { name :: String, serving :: String }
menuLines = mapWithIndex \i guest -> { name: guest.name, serving: (if i == 0 then "" else ", ") <> guest.name <> "’s " <> guest.dish }

servingText :: { name :: String, serving :: String } -> { value :: String }
servingText line = { value: line.serving }
