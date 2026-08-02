module ColorMixerMDC2 (colorMixerMDC2) where

import Prelude ((#), ($), (<>), (<<<), (==), (>>>), Unit, const, max, min, show)

import Data.Array (find)
import Data.Int (hexadecimal, round, toStringAs)
import Data.Maybe (Maybe(..), maybe)
import Data.Profunctor.Row.RecordToRecord (pempty)
import Data.String (length, toUpper)
import Data.Variant (match)
import Effect (Effect)
import PUI (asField, completed, constantly, foreach, mvu, projected, tapped, toCase, updated)
import PUI.Web.HTML (attrWith, body, clicked, div, text, (:=))
import PUI.Web.MDC2 (body2, card, elevation20, sliderLive)
import QualifiedDo.Semigroupoid as Semigroupoid

chipStyle :: { mix :: { red :: Number, green :: Number, blue :: Number } } -> String
chipStyle p = "width: 36px; height: 36px; border-radius: 50%; cursor: pointer; border: 1px solid #999; background-color: " <> rgb p.mix <> ";"

swatchStyle :: { red :: { current :: Number, min :: Number, max :: Number, step :: Maybe Number }, green :: { current :: Number, min :: Number, max :: Number, step :: Maybe Number }, blue :: { current :: Number, min :: Number, max :: Number, step :: Maybe Number } } -> String
swatchStyle channels = "width: 100%; max-width: 420px; height: 120px; border-radius: 8px; border: 1px solid #ccc; background-color: " <> rgb (mixOf channels) <> ";"

colorMixerMDC2 :: Effect Unit
colorMixerMDC2 =
  body $
    elevation20 $
      card { caption: "Color Mixer" } $ ( Semigroupoid.do
          sliderLive { label: "Red" } # asField @"red" # completed
          sliderLive { label: "Green" } # asField @"green" # completed
          sliderLive { label: "Blue" } # asField @"blue" # completed
          ( div >>> "style" := "margin: 10px 0;" $ Semigroupoid.do
              attrWith "style" swatchStyle $ div $ pempty # constantly {}
              div >>> "style" := "display: flex; gap: 8px; margin-top: 10px;" $
                ( clicked ( div >>> attrWith "title" _.name >>> attrWith "style" (\p -> chipStyle { mix: p.mix }) $ pempty # constantly {} ) ) # foreach @"name" (const palette)) # toCase @"preset" _.name # updated (match { preset: applyPreset })
          body2 text # projected hexText # tapped
          body2 text # projected rgbText # tapped
      ) # mvu duskViolet

applyPreset :: String -> { red :: { current :: Number, min :: Number, max :: Number, step :: Maybe Number }, green :: { current :: Number, min :: Number, max :: Number, step :: Maybe Number }, blue :: { current :: Number, min :: Number, max :: Number, step :: Maybe Number } } -> { red :: { current :: Number, min :: Number, max :: Number, step :: Maybe Number }, green :: { current :: Number, min :: Number, max :: Number, step :: Maybe Number }, blue :: { current :: Number, min :: Number, max :: Number, step :: Maybe Number } }
applyPreset name channels = maybe channels
  (\p -> { red: channels.red { current = p.mix.red }, green: channels.green { current = p.mix.green }, blue: channels.blue { current = p.mix.blue } })
  (find (\p -> p.name == name) palette)

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

mixOf :: { red :: { current :: Number, min :: Number, max :: Number, step :: Maybe Number }, green :: { current :: Number, min :: Number, max :: Number, step :: Maybe Number }, blue :: { current :: Number, min :: Number, max :: Number, step :: Maybe Number } } -> { red :: Number, green :: Number, blue :: Number }
mixOf { red, green, blue } = { red: red.current, green: green.current, blue: blue.current }


hexText :: { red :: { current :: Number, min :: Number, max :: Number, step :: Maybe Number }, green :: { current :: Number, min :: Number, max :: Number, step :: Maybe Number }, blue :: { current :: Number, min :: Number, max :: Number, step :: Maybe Number } } -> String
hexText = hex <<< mixOf

rgbText :: { red :: { current :: Number, min :: Number, max :: Number, step :: Maybe Number }, green :: { current :: Number, min :: Number, max :: Number, step :: Maybe Number }, blue :: { current :: Number, min :: Number, max :: Number, step :: Maybe Number } } -> String
rgbText = rgb <<< mixOf

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

duskViolet :: { red :: { current :: Number, min :: Number, max :: Number, step :: Maybe Number }, green :: { current :: Number, min :: Number, max :: Number, step :: Maybe Number }, blue :: { current :: Number, min :: Number, max :: Number, step :: Maybe Number } }
duskViolet = let m = mix 96.0 64.0 160.0 in { red: channelRange m.red, green: channelRange m.green, blue: channelRange m.blue }

channelRange :: Number -> { current :: Number, min :: Number, max :: Number, step :: Maybe Number }
channelRange n = { current: n, min: minChannel, max: maxChannel, step: Just 1.0 }

minChannel :: Number
minChannel = 0.0

maxChannel :: Number
maxChannel = 255.0
