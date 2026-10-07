module ColorMixerViewModel (mixedColor, applyPreset, duskViolet, hexLine, palette, presetAppliedLine, rgb, rgbLine) where

import Prelude ((<>), (<<<), (==), max, min, show)

import Data.Array (find)
import Data.Int (hexadecimal, round, toStringAs)
import Data.Maybe (maybe)
import Data.String (length, toUpper)

duskViolet :: { "Blue" :: { current :: Number, max :: Number, min :: Number, step :: [ continuous :: {}, discrete :: Number ] }, "Green" :: { current :: Number, max :: Number, min :: Number, step :: [ continuous :: {}, discrete :: Number ] }, "Red" :: { current :: Number, max :: Number, min :: Number, step :: [ continuous :: {}, discrete :: Number ] } }
duskViolet = let m = mix 96.0 64.0 160.0 in { "Red": channelRange m."Red", "Green": channelRange m."Green", "Blue": channelRange m."Blue" }

hexLine :: { "Blue" :: { current :: Number, max :: Number, min :: Number, step :: [ continuous :: {}, discrete :: Number ] }, "Green" :: { current :: Number, max :: Number, min :: Number, step :: [ continuous :: {}, discrete :: Number ] }, "Red" :: { current :: Number, max :: Number, min :: Number, step :: [ continuous :: {}, discrete :: Number ] } } -> String
hexLine channels = hex (mixOf channels)

rgbLine :: { "Blue" :: { current :: Number, max :: Number, min :: Number, step :: [ continuous :: {}, discrete :: Number ] }, "Green" :: { current :: Number, max :: Number, min :: Number, step :: [ continuous :: {}, discrete :: Number ] }, "Red" :: { current :: Number, max :: Number, min :: Number, step :: [ continuous :: {}, discrete :: Number ] } } -> String
rgbLine channels = rgb (mixOf channels)

applyPreset :: { event :: String, model :: { "Blue" :: { current :: Number, max :: Number, min :: Number, step :: [ continuous :: {}, discrete :: Number ] }, "Green" :: { current :: Number, max :: Number, min :: Number, step :: [ continuous :: {}, discrete :: Number ] }, "Red" :: { current :: Number, max :: Number, min :: Number, step :: [ continuous :: {}, discrete :: Number ] } } } -> { "Blue" :: { current :: Number, max :: Number, min :: Number, step :: [ continuous :: {}, discrete :: Number ] }, "Green" :: { current :: Number, max :: Number, min :: Number, step :: [ continuous :: {}, discrete :: Number ] }, "Red" :: { current :: Number, max :: Number, min :: Number, step :: [ continuous :: {}, discrete :: Number ] } }
applyPreset { event: name, model: channels } = maybe channels
  (\p -> channels { "Red" = channels."Red" { current = p.mix."Red" }, "Green" = channels."Green" { current = p.mix."Green" }, "Blue" = channels."Blue" { current = p.mix."Blue" } })
  (find (\p -> p.name == name) palette)

palette :: Array { mix :: { "Blue" :: Number, "Green" :: Number, "Red" :: Number }, name :: String }
palette =
  [ { name: "White", mix: mix 255.0 255.0 255.0 }
  , { name: "Black", mix: mix 0.0 0.0 0.0 }
  , { name: "Crimson", mix: mix 220.0 20.0 60.0 }
  , { name: "Leaf", mix: mix 76.0 175.0 80.0 }
  , { name: "Sky", mix: mix 33.0 150.0 243.0 }
  ]

mix :: Number -> Number -> Number -> { "Red" :: Number, "Green" :: Number, "Blue" :: Number }
mix red green blue = { "Red": clampChannel red, "Green": clampChannel green, "Blue": clampChannel blue }

mixedColor :: { "Blue" :: { current :: Number, max :: Number, min :: Number, step :: [ continuous :: {}, discrete :: Number ] }, "Green" :: { current :: Number, max :: Number, min :: Number, step :: [ continuous :: {}, discrete :: Number ] }, "Red" :: { current :: Number, max :: Number, min :: Number, step :: [ continuous :: {}, discrete :: Number ] } } -> String
mixedColor = rgb <<< mixOf

mixOf :: forall r1. { "Red" :: { current :: Number, min :: Number, max :: Number, step :: [ discrete :: Number, continuous :: {} ] }, "Green" :: { current :: Number, min :: Number, max :: Number, step :: [ discrete :: Number, continuous :: {} ] }, "Blue" :: { current :: Number, min :: Number, max :: Number, step :: [ discrete :: Number, continuous :: {} ] } | r1 } -> { "Red" :: Number, "Green" :: Number, "Blue" :: Number }
mixOf { "Red": red, "Green": green, "Blue": blue } = { "Red": red.current, "Green": green.current, "Blue": blue.current }

hex :: forall r1. { "Red" :: Number, "Green" :: Number, "Blue" :: Number | r1 } -> String
hex { "Red": red, "Green": green, "Blue": blue } = "#" <> channelHex red <> channelHex green <> channelHex blue

channelHex :: Number -> String
channelHex n =
  let digits = toUpper (toStringAs hexadecimal (round (clampChannel n)))
  in if length digits == 1 then "0" <> digits else digits

rgb :: { "Blue" :: Number, "Green" :: Number, "Red" :: Number } -> String
rgb { "Red": red, "Green": green, "Blue": blue } = "rgb(" <> channel red <> ", " <> channel green <> ", " <> channel blue <> ")"

channel :: Number -> String
channel = show <<< round <<< clampChannel

clampChannel :: Number -> Number
clampChannel = max minChannel <<< min maxChannel

channelRange :: Number -> { current :: Number, min :: Number, max :: Number, step :: [ discrete :: Number, continuous :: {} ] }
channelRange n = { current: n, min: minChannel, max: maxChannel, step: .discrete 1.0 }

minChannel :: Number
minChannel = 0.0

maxChannel :: Number
maxChannel = 255.0

presetAppliedLine :: { "Blue" :: { current :: Number, max :: Number, min :: Number, step :: [ continuous :: {}, discrete :: Number ] }, "Green" :: { current :: Number, max :: Number, min :: Number, step :: [ continuous :: {}, discrete :: Number ] }, "Red" :: { current :: Number, max :: Number, min :: Number, step :: [ continuous :: {}, discrete :: Number ] } } -> String
presetAppliedLine channels = "Now mixing " <> hexLine channels
