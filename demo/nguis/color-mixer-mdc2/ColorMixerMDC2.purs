module ColorMixerMDC2 (colorMixerMDC2) where

import Prelude ((#), ($), (<>), (<<<), (==), (>>>), Unit, const, max, min, show)

import Data.Array (find)
import Data.Int (hexadecimal, round, toStringAs)
import Data.Maybe (maybe)
import Data.Profunctor (lcmap, rmap)
import Data.Profunctor.Row.RecordToRecord (pempty)
import Data.String (length, toUpper)
import Data.Variant (match)
import Effect (Effect)
import PUI (asField, completed, foreach, mvu, projection, tapped, toCase, updates)
import PUI.HTML (attrWith, body, clicked, div, text, (:=))
import PUI.MDC2 (body2, card, elevation20, sliderLive)
import QualifiedDo.Semigroupoid as Semigroupoid

chipStyle :: { mix :: { red :: Number, green :: Number, blue :: Number } } -> String
chipStyle p = "width: 36px; height: 36px; border-radius: 50%; cursor: pointer; border: 1px solid #999; background-color: " <> rgb p.mix <> ";"

swatchStyle :: { red :: Number, green :: Number, blue :: Number } -> String
swatchStyle { red, green, blue } = "width: 100%; max-width: 420px; height: 120px; border-radius: 8px; border: 1px solid #ccc; background-color: " <> rgb { red, green, blue } <> ";"

colorMixerMDC2 :: Effect Unit
colorMixerMDC2 =
  body $
    elevation20 $
      card { caption: "Color Mixer" } $ ( Semigroupoid.do
          sliderLive { label: "Red", min: minChannel, max: maxChannel, step: channelStep } # asField @"red" # completed
          sliderLive { label: "Green", min: minChannel, max: maxChannel, step: channelStep } # asField @"green" # completed
          sliderLive { label: "Blue", min: minChannel, max: maxChannel, step: channelStep } # asField @"blue" # completed
          ( div >>> "style" := "margin: 10px 0;" $ Semigroupoid.do
              attrWith "style" swatchStyle $ div $ pempty # lcmap (const {})
              div >>> "style" := "display: flex; gap: 8px; margin-top: 10px;" $
                ( clicked ( div >>> attrWith "title" _.name >>> attrWith "style" (\p -> chipStyle { mix: p.mix }) $ pempty # lcmap (const {}) ) # rmap _.name ) # foreach @"name" # lcmap (const palette)) # toCase @"preset" # updates (match { preset: applyPreset })
          body2 text # projection hex # tapped
          body2 text # projection rgb # tapped
      ) # mvu duskViolet

applyPreset :: String -> { red :: Number, green :: Number, blue :: Number } -> { red :: Number, green :: Number, blue :: Number }
applyPreset name current = maybe current _.mix (find (\p -> p.name == name) palette)

palette :: Array { name :: String, mix :: { red :: Number, green :: Number, blue :: Number } }
palette =
  [ { name: "White", mix: mix 255.0 255.0 255.0 }
  , { name: "Black", mix: mix 0.0 0.0 0.0 }
  , { name: "Crimson", mix: mix 220.0 20.0 60.0 }
  , { name: "Leaf", mix: mix 76.0 175.0 80.0 }
  , { name: "Sky", mix: mix 33.0 150.0 243.0 }
  ]

mix :: Number -> Number -> Number -> { red :: Number, green :: Number, blue :: Number }
mix red green blue = { red: clampChannel red, green: clampChannel green, blue: clampChannel blue }

hex :: { red :: Number, green :: Number, blue :: Number } -> String
hex { red, green, blue } = "#" <> channelHex red <> channelHex green <> channelHex blue

channelHex :: Number -> String
channelHex n =
  let digits = toUpper (toStringAs hexadecimal (round (clampChannel n)))
  in if length digits == 1 then "0" <> digits else digits

rgb :: { red :: Number, green :: Number, blue :: Number } -> String
rgb { red, green, blue } = "rgb(" <> channel red <> ", " <> channel green <> ", " <> channel blue <> ")"

channel :: Number -> String
channel = show <<< round <<< clampChannel

clampChannel :: Number -> Number
clampChannel = max minChannel <<< min maxChannel

duskViolet :: { red :: Number, green :: Number, blue :: Number }
duskViolet = mix 96.0 64.0 160.0

minChannel :: Number
minChannel = 0.0

maxChannel :: Number
maxChannel = 255.0

channelStep :: Number
channelStep = 1.0
