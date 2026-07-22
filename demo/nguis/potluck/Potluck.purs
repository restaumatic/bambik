module Potluck (potluck) where

import Prelude ((#), ($), (<>), (==), Unit, show)

import Data.Array (length, mapWithIndex, zipWith)
import Data.Maybe (Maybe(..))
import Data.Profunctor (lcmap)
import Data.Profunctor.Acting (acted)
import Effect (Effect)
import PUI (asField, displayed, projection, tapped, with)
import PUI.HTML (body, foreach, text)
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
                segmentedButton (labeled dishes) # asField @"dish" # lcmap pickOf
            ) # acted _.name
          headline6 $ (text # lcmap servingText) # foreach _.name # lcmap menuLines
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

pickOf :: Guest -> { dish :: Maybe String }
pickOf guest = { dish: guest.dish }

callToAction :: Array Guest -> String
callToAction guests = show (length guests) <> " guests invited — everyone picks one dish; the menu prints once the table is complete."

menuLines :: Array { dish :: String } -> Array { name :: String, serving :: String }
menuLines picks = mapWithIndex sentence (zipWith (\g p -> { name: g.name, dish: p.dish }) invited picks)
  where
  sentence i pick = { name: pick.name, serving: (if i == 0 then "On the table: " else ", ") <> pick.name <> "’s " <> pick.dish }

servingText :: { name :: String, serving :: String } -> { value :: String }
servingText line = { value: line.serving }
