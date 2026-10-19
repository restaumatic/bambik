module ColorMixer (Channel, Mix, Mixer, applyPreset, duskViolet, hexLine, mixedColor, palette, rgb, rgbLine) where

import Prelude ((<>), (<<<), (==), max, min, show)

import Data.Array (find)
import Data.Int (hexadecimal, round, toStringAs)
import Data.Maybe (maybe)
import Data.String (length, toUpper)

type Mixer = { "Red" :: Channel, "Green" :: Channel, "Blue" :: Channel }

type Channel =
  { current :: Number
  , min :: Number
  , max :: Number
  , step :: [ discrete :: Number, continuous :: {} ]
  }

type Mix = { "Red" :: Number, "Green" :: Number, "Blue" :: Number }

duskViolet :: Mixer
duskViolet = { "Red": channelRange 96.0, "Green": channelRange 64.0, "Blue": channelRange 160.0 }

hexLine :: Mixer -> String
hexLine channels = hex (mixOf channels)

rgbLine :: Mixer -> String
rgbLine channels = rgb (mixOf channels)

applyPreset :: { event :: String, model :: Mixer } -> Mixer
applyPreset { event: name, model: channels } = maybe channels mixed (find (\p -> p.name == name) palette)
  where
  mixed { mix: m } = channels
    { "Red" = channels."Red" { current = m."Red" }
    , "Green" = channels."Green" { current = m."Green" }
    , "Blue" = channels."Blue" { current = m."Blue" }
    }

palette :: Array { mix :: Mix, name :: String }
palette =
  [ { name: "White", mix: mix 255.0 255.0 255.0 }
  , { name: "Black", mix: mix 0.0 0.0 0.0 }
  , { name: "Crimson", mix: mix 220.0 20.0 60.0 }
  , { name: "Leaf", mix: mix 76.0 175.0 80.0 }
  , { name: "Sky", mix: mix 33.0 150.0 243.0 }
  ]

mix :: Number -> Number -> Number -> Mix
mix red green blue = { "Red": clampChannel red, "Green": clampChannel green, "Blue": clampChannel blue }

mixedColor :: Mixer -> String
mixedColor = rgb <<< mixOf

mixOf :: Mixer -> Mix
mixOf { "Red": red, "Green": green, "Blue": blue } = { "Red": red.current, "Green": green.current, "Blue": blue.current }

hex :: Mix -> String
hex { "Red": red, "Green": green, "Blue": blue } = "#" <> channelHex red <> channelHex green <> channelHex blue

channelHex :: Number -> String
channelHex n =
  let digits = toUpper (toStringAs hexadecimal (round (clampChannel n)))
  in if length digits == 1 then "0" <> digits else digits

rgb :: Mix -> String
rgb { "Red": red, "Green": green, "Blue": blue } = "rgb(" <> channel red <> ", " <> channel green <> ", " <> channel blue <> ")"

channel :: Number -> String
channel = show <<< round <<< clampChannel

clampChannel :: Number -> Number
clampChannel = max minChannel <<< min maxChannel

channelRange :: Number -> Channel
channelRange n = { current: n, min: minChannel, max: maxChannel, step: .discrete 1.0 }

minChannel :: Number
minChannel = 0.0

maxChannel :: Number
maxChannel = 255.0
