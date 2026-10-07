module ColorMixerMDC2 (colorMixerMDC2) where

import Prelude ((#), ($), (<>), (>>>), Unit, const)

import ColorMixerViewModel (applyPreset, duskViolet, hexLine, mixedColor, palette, rgb, rgbLine)
import Effect (Effect)
import PUI (blank, blankStatus, fold, foreach, joined, looped, with)
import PUI.Web (attrWith, clicked, shown, text, (:=))
import PUI.Web.HTML (div)
import PUI.Web.MDC2 (body, body2, sliderLive)
import QualifiedDo.Semigroupoid as Semigroupoid

colorMixerMDC2 :: Effect Unit
colorMixerMDC2 =
  body $
    Semigroupoid.do
      sliderLive @"Red" {}
      sliderLive @"Green" {}
      sliderLive @"Blue" {}
      ( body2 $ text hexLine ) # shown
      ( body2 $ text rgbLine ) # shown
      ( div $ Semigroupoid.do
        div >>> attrWith "style" swatchStyle $ blank
        div >>> "style" := "display: flex; gap: 8px; margin-top: 10px;" $
          clicked @"Preset applied" _.name ( div >>> attrWith "title" _.name >>> attrWith "style" chipFace $ blank ) # foreach @"name"
            @( name :: String
             , mix :: { "Red" :: Number, "Green" :: Number, "Blue" :: Number }
             ) (const palette) ) # joined @"Preset applied"
      blankStatus @"Preset applied" # fold applyPreset
    # looped
      @( "Red" :: { current :: Number, min :: Number, max :: Number, step :: [ discrete :: Number, continuous :: {} ] }
       , "Green" :: { current :: Number, min :: Number, max :: Number, step :: [ discrete :: Number, continuous :: {} ] }
       , "Blue" :: { current :: Number, min :: Number, max :: Number, step :: [ discrete :: Number, continuous :: {} ] }
       ) # with duskViolet

chipFace :: { name :: String, mix :: { "Red" :: Number, "Green" :: Number, "Blue" :: Number } } -> String
chipFace { mix } = "width: 36px; height: 36px; border-radius: 50%; cursor: pointer; border: 1px solid #999; background-color: " <> rgb mix <> ";"

swatchStyle :: { "Red" :: { current :: Number, min :: Number, max :: Number, step :: [ discrete :: Number, continuous :: {} ] }, "Green" :: { current :: Number, min :: Number, max :: Number, step :: [ discrete :: Number, continuous :: {} ] }, "Blue" :: { current :: Number, min :: Number, max :: Number, step :: [ discrete :: Number, continuous :: {} ] } } -> String
swatchStyle channels = "width: 100%; max-width: 420px; height: 120px; border-radius: 8px; border: 1px solid #ccc; background-color: " <> mixedColor channels <> ";"
