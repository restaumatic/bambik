module ColorMixer.MDC3 (colorMixer) where

import Prelude ((#), ($), (<>), (>>>), Unit, const, identity)

import ColorMixer (applyPreset, duskViolet, hexLine, mixedColor, palette, rgb, rgbLine)
import Effect (Effect)
import PUI (anyOf, fold, joined, looped, with)
import PUI.Web (attrAt, attrWith, text, (:=))
import PUI.Web.HTML (div)
import PUI.Web.MDC3 (body, bodyMedium, sliderLive, tile)
import QualifiedDo.Semigroupoid as Semigroupoid

colorMixer :: Effect Unit
colorMixer =
  body $ Semigroupoid.do
    ( Semigroupoid.do
      sliderLive @"Red"
      sliderLive @"Green"
      sliderLive @"Blue"
    ) # looped
    bodyMedium $ text hexLine
    bodyMedium $ text rgbLine
    div >>> attrWith "style" swatchStyle $ identity
    ( div >>> "style" := "display: flex; gap: 8px; margin-top: 10px;" $
      tile @"Preset applied" @"name" identity # attrAt @"name" "aria-label" # attrAt @"name" "title" # attrWith "style" chipFace # anyOf @"name" (const palette) ) # joined @"Preset applied"
    fold @"Preset applied" applyPreset
  # looped # with duskViolet

chipFace :: { mix :: { "Blue" :: Number, "Green" :: Number, "Red" :: Number }, name :: String } -> String
chipFace { mix } = "width: 36px; height: 36px; border-radius: 50%; cursor: pointer; border: 1px solid #999; background-color: " <> rgb mix <> ";"

swatchStyle
  :: { "Blue" :: { current :: Number, max :: Number, min :: Number, step :: [ continuous :: {}, discrete :: Number ] }
     , "Green" :: { current :: Number, max :: Number, min :: Number, step :: [ continuous :: {}, discrete :: Number ] }
     , "Red" :: { current :: Number, max :: Number, min :: Number, step :: [ continuous :: {}, discrete :: Number ] }
     }
  -> String
swatchStyle channels = "width: 100%; max-width: 420px; height: 120px; border-radius: 8px; border: 1px solid #ccc; background-color: " <> mixedColor channels <> ";"
