module ColorMixer (colorMixer) where

import Prelude ((#), ($), (<>), (<<<), (==), (>>>), Unit, max, min, show)

import Data.Array (find)
import Data.Int (hexadecimal, round, toStringAs)
import Data.Maybe (maybe)
import Data.Profunctor.Row.RecordToRecord (pempty)
import Data.Profunctor.Row.RecordToRecord as RecordToRecord
import Data.String (length, toUpper)
import Data.Variant (match)
import Effect (Effect)
import PUI (asField, completed, forValue, mvu, projection, tapped, toCase, updates)
import PUI.HTML (body, div, dynamic, each, onKeyClicked, text, (:=))
import PUI.MDC (body2, card, elevation20, sliderLive)
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
          ( div >>> "style" := "margin: 10px 0;" $
              ( onKeyClicked
                  ( dynamic \m -> RecordToRecord.do
                      div >>> "id" := "swatch"
                        >>> "style"
                          := ( "width: 100%; max-width: 420px; height: 120px; border-radius: 8px; "
                                <> "border: 1px solid #ccc; background-color: " <> rgb m <> ";"
                            ) $ pempty
                      div >>> "style" := "display: flex; gap: 8px; margin-top: 10px;" $
                        each palette \p ->
                          div >>> "data-key" := p.name >>> "class" := "preset" >>> "title" := p.name
                            >>> "style"
                              := ( "width: 36px; height: 36px; border-radius: 50%; cursor: pointer; "
                                    <> "border: 1px solid #999; background-color: " <> rgb p.mix <> ";"
                                ) $ pempty
                  ) # toCase @"preset"
              )
          ) # updates (match { preset: applyPreset })
          body2 (text # projection hex # forValue) # tapped
          body2 (text # projection rgb # forValue) # tapped
      ) # mvu duskViolet

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
