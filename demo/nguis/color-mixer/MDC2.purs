module ColorMixer.MDC2 (colorMixer) where

import Prelude ((#), ($), (<>), (>>>), Unit, const, identity)

import ColorMixer (applyPreset, duskViolet, hexLine, mixedColor, palette, rgb, rgbLine)
import Effect (Effect)
import PUI (anyOf, fold, joined, looped, with)
import PUI.Web (attrAt, attrWith, clicked, text, (:=))
import PUI.Web.HTML (div)
import PUI.Web.MDC2 (body, body2, sliderLive)
import QualifiedDo.Semigroupoid as Semigroupoid

colorMixer :: Effect Unit
colorMixer =
  body $ Semigroupoid.do
    sliderLive @"Red"
    sliderLive @"Green"
    sliderLive @"Blue"
    body2 $ text hexLine
    body2 $ text rgbLine
    ( div $ Semigroupoid.do
      div >>> attrWith "style" swatchStyle $ identity
      div >>> "style" := "display: flex; gap: 8px; margin-top: 10px;" $
        ( clicked @"Preset applied" @"name" $ div >>> attrAt @"name" "title" >>> attrWith "style" chipFace $ identity ) # anyOf @"name"
          @( name :: String
           , mix :: { "Red" :: Number, "Green" :: Number, "Blue" :: Number }
           ) (const palette) ) # joined
    fold @"Preset applied" applyPreset
  # looped
    @( "Red" :: { current :: Number, min :: Number, max :: Number, step :: [ discrete :: Number, continuous :: {} ] }
     , "Green" :: { current :: Number, min :: Number, max :: Number, step :: [ discrete :: Number, continuous :: {} ] }
     , "Blue" :: { current :: Number, min :: Number, max :: Number, step :: [ discrete :: Number, continuous :: {} ] }
     ) # with duskViolet

chipFace :: { name :: String, mix :: { "Red" :: Number, "Green" :: Number, "Blue" :: Number } } -> String
chipFace { mix } = "width: 36px; height: 36px; border-radius: 50%; cursor: pointer; border: 1px solid #999; background-color: " <> rgb mix <> ";"

swatchStyle :: { "Red" :: { current :: Number, min :: Number, max :: Number, step :: [ discrete :: Number, continuous :: {} ] }, "Green" :: { current :: Number, min :: Number, max :: Number, step :: [ discrete :: Number, continuous :: {} ] }, "Blue" :: { current :: Number, min :: Number, max :: Number, step :: [ discrete :: Number, continuous :: {} ] } } -> String
swatchStyle channels = "width: 100%; max-width: 420px; height: 120px; border-radius: 8px; border: 1px solid #ccc; background-color: " <> mixedColor channels <> ";"
