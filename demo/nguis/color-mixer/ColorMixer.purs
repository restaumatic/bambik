module ColorMixer (colorMixer) where

import Prelude ((#), ($), (<#>), (<>), (<<<), (==), Unit, max, min, show)

import Data.Array (find)
import Data.Int (hexadecimal, round, toStringAs)
import Data.Maybe (maybe)
import Data.String (length, toUpper)
import Data.Tuple (Tuple(..))
import Data.Variant (match)
import Effect (Effect)
import PUI (asField, completed, forValue, mvu, projection, tapped, updates)
import PUI.HTML (Markup(..), body, text, view)
import PUI.MDC (body2, card, elevation20, sliderLive)
import PUI.Web (onKeyClick)
import QualifiedDo.Semigroupoid as Semigroupoid

type Mix = { red :: Number, green :: Number, blue :: Number }

colorMixer :: Effect Unit
colorMixer =
  body $
    elevation20 $
      card { caption: "Color Mixer" } $ ( Semigroupoid.do
          sliderLive { label: "Red", min: minChannel, max: maxChannel, step: channelStep }
            # asField @"red" # completed
          sliderLive { label: "Green", min: minChannel, max: maxChannel, step: channelStep }
            # asField @"green" # completed
          sliderLive { label: "Blue", min: minChannel, max: maxChannel, step: channelStep }
            # asField @"blue" # completed
          view
            """<div style="margin: 10px 0;"></div>"""
            renderSwatch
            (\node emit -> onKeyClick node \key -> emit (.preset key :: [ preset :: String ]))
            # updates (match { preset: applyPreset })
          body2 (text # projection hex # forValue) # tapped
          body2 (text # projection rgb # forValue) # tapped
      ) # mvu duskViolet

renderSwatch :: Mix -> Array Markup
renderSwatch m =
  [ Element "div"
      [ Tuple "id" "swatch"
      , Tuple "style"
          ( "width: 100%; max-width: 420px; height: 120px; border-radius: 8px; "
              <> "border: 1px solid #ccc; background-color: " <> rgb m <> ";"
          )
      ]
      []
  , Element "div"
      [ Tuple "style" "display: flex; gap: 8px; margin-top: 10px;" ]
      (palette <#> chip)
  ]
  where
  chip p =
    Element "div"
      [ Tuple "data-key" p.name
      , Tuple "class" "preset"
      , Tuple "title" p.name
      , Tuple "style"
          ( "width: 36px; height: 36px; border-radius: 50%; cursor: pointer; "
              <> "border: 1px solid #999; background-color: " <> rgb p.mix <> ";"
          )
      ]
      []

applyPreset :: String -> Mix -> Mix
applyPreset name current = maybe current _.mix (find (\p -> p.name == name) palette)

palette :: Array { name :: String, mix :: Mix }
palette =
  [ { name: "White", mix: mix 255.0 255.0 255.0 }
  , { name: "Black", mix: mix 0.0 0.0 0.0 }
  , { name: "Crimson", mix: mix 220.0 20.0 60.0 }
  , { name: "Leaf", mix: mix 76.0 175.0 80.0 }
  , { name: "Sky", mix: mix 33.0 150.0 243.0 }
  ]

mix :: Number -> Number -> Number -> Mix
mix red green blue = { red: clampChannel red, green: clampChannel green, blue: clampChannel blue }

hex :: Mix -> String
hex m = "#" <> channelHex m.red <> channelHex m.green <> channelHex m.blue

channelHex :: Number -> String
channelHex n =
  let digits = toUpper (toStringAs hexadecimal (round (clampChannel n)))
  in if length digits == 1 then "0" <> digits else digits

rgb :: Mix -> String
rgb m = "rgb(" <> channel m.red <> ", " <> channel m.green <> ", " <> channel m.blue <> ")"

channel :: Number -> String
channel = show <<< round <<< clampChannel

clampChannel :: Number -> Number
clampChannel = max minChannel <<< min maxChannel

duskViolet :: Mix
duskViolet = mix 96.0 64.0 160.0

minChannel :: Number
minChannel = 0.0

maxChannel :: Number
maxChannel = 255.0

channelStep :: Number
channelStep = 1.0
