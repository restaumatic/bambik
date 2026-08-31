module ColorMixerLogic (applyPreset, duskViolet, mixOf, palette, presentColorMixer, rgb) where

import Prelude ((<>), (<<<), (==), max, min, show)

import Data.Array (find)
import Data.Int (hexadecimal, round, toStringAs)
import Data.Maybe (maybe)
import Data.String (length, toUpper)

duskViolet :: { "Red" :: { current :: Number, min :: Number, max :: Number, step :: [ discrete :: Number, continuous :: {} ] }, "Green" :: { current :: Number, min :: Number, max :: Number, step :: [ discrete :: Number, continuous :: {} ] }, "Blue" :: { current :: Number, min :: Number, max :: Number, step :: [ discrete :: Number, continuous :: {} ] }, hexText :: String, rgbText :: String }
duskViolet = let m = mix 96.0 64.0 160.0 in presentColorMixer { "Red": channelRange m."Red", "Green": channelRange m."Green", "Blue": channelRange m."Blue", hexText: "", rgbText: "" }

presentColorMixer :: { "Red" :: { current :: Number, min :: Number, max :: Number, step :: [ discrete :: Number, continuous :: {} ] }, "Green" :: { current :: Number, min :: Number, max :: Number, step :: [ discrete :: Number, continuous :: {} ] }, "Blue" :: { current :: Number, min :: Number, max :: Number, step :: [ discrete :: Number, continuous :: {} ] }, hexText :: String, rgbText :: String } -> { "Red" :: { current :: Number, min :: Number, max :: Number, step :: [ discrete :: Number, continuous :: {} ] }, "Green" :: { current :: Number, min :: Number, max :: Number, step :: [ discrete :: Number, continuous :: {} ] }, "Blue" :: { current :: Number, min :: Number, max :: Number, step :: [ discrete :: Number, continuous :: {} ] }, hexText :: String, rgbText :: String }
presentColorMixer r = let m = mixOf { "Red": r."Red", "Green": r."Green", "Blue": r."Blue" } in r { hexText = hex m, rgbText = rgb m }

applyPreset :: String -> { "Red" :: { current :: Number, min :: Number, max :: Number, step :: [ discrete :: Number, continuous :: {} ] }, "Green" :: { current :: Number, min :: Number, max :: Number, step :: [ discrete :: Number, continuous :: {} ] }, "Blue" :: { current :: Number, min :: Number, max :: Number, step :: [ discrete :: Number, continuous :: {} ] } } -> { "Red" :: { current :: Number, min :: Number, max :: Number, step :: [ discrete :: Number, continuous :: {} ] }, "Green" :: { current :: Number, min :: Number, max :: Number, step :: [ discrete :: Number, continuous :: {} ] }, "Blue" :: { current :: Number, min :: Number, max :: Number, step :: [ discrete :: Number, continuous :: {} ] } }
applyPreset name channels = maybe channels
  (\p -> { "Red": channels."Red" { current = p.mix."Red" }, "Green": channels."Green" { current = p.mix."Green" }, "Blue": channels."Blue" { current = p.mix."Blue" } })
  (find (\p -> p.name == name) palette)

palette :: Array { name :: String, mix :: { "Red" :: Number, "Green" :: Number, "Blue" :: Number } }
palette =
  [ { name: "White", mix: mix 255.0 255.0 255.0 }
  , { name: "Black", mix: mix 0.0 0.0 0.0 }
  , { name: "Crimson", mix: mix 220.0 20.0 60.0 }
  , { name: "Leaf", mix: mix 76.0 175.0 80.0 }
  , { name: "Sky", mix: mix 33.0 150.0 243.0 }
  ]

mix :: Number -> Number -> Number -> { "Red" :: Number, "Green" :: Number, "Blue" :: Number }
mix red green blue = { "Red": clampChannel red, "Green": clampChannel green, "Blue": clampChannel blue }

mixOf :: { "Red" :: { current :: Number, min :: Number, max :: Number, step :: [ discrete :: Number, continuous :: {} ] }, "Green" :: { current :: Number, min :: Number, max :: Number, step :: [ discrete :: Number, continuous :: {} ] }, "Blue" :: { current :: Number, min :: Number, max :: Number, step :: [ discrete :: Number, continuous :: {} ] } } -> { "Red" :: Number, "Green" :: Number, "Blue" :: Number }
mixOf { "Red": red, "Green": green, "Blue": blue } = { "Red": red.current, "Green": green.current, "Blue": blue.current }

hex :: { "Red" :: Number, "Green" :: Number, "Blue" :: Number } -> String
hex { "Red": red, "Green": green, "Blue": blue } = "#" <> channelHex red <> channelHex green <> channelHex blue

channelHex :: Number -> String
channelHex n =
  let digits = toUpper (toStringAs hexadecimal (round (clampChannel n)))
  in if length digits == 1 then "0" <> digits else digits

rgb :: { "Red" :: Number, "Green" :: Number, "Blue" :: Number } -> String
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
