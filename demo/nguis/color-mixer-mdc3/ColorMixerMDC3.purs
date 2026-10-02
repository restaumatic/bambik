module ColorMixerMDC3 (colorMixerMDC3) where

import Prelude ((#), ($), (<>), (>>>), Unit, const)

import ColorMixerViewModel (applyPreset, duskViolet, hexLine, mixedColor, palette, rgb, rgbLine)
import Data.Variant (match)
import Effect (Effect)
import PUI (blank, foreach, mvu, updated)
import PUI.Web (attrWith, clicked, shown, text, (:=))
import PUI.Web.HTML (div)
import PUI.Web.MDC3 (body, bodyMedium, sliderLive)
import QualifiedDo.Semigroupoid as Semigroupoid

colorMixerMDC3 :: Effect Unit
colorMixerMDC3 =
  body $
    ( Semigroupoid.do
      sliderLive @"Red" {}
      sliderLive @"Green" {}
      sliderLive @"Blue" {}
      ( div $ Semigroupoid.do
        div >>> attrWith "style" swatchStyle $ blank
        div >>> "style" := "display: flex; gap: 8px; margin-top: 10px;" $
          clicked @"preset" _.name ( div >>> attrWith "title" _.name >>> attrWith "style" chipFace $ blank ) # foreach @"name" @( name :: String, mix :: { "Red" :: Number, "Green" :: Number, "Blue" :: Number } ) (const palette) ) # updated (match { preset: applyPreset })
      ( bodyMedium $ text hexLine ) # shown
      ( bodyMedium $ text rgbLine ) # shown
    ) # mvu
      @( "Red" :: { current :: Number, min :: Number, max :: Number, step :: [ discrete :: Number, continuous :: {} ] }
       , "Green" :: { current :: Number, min :: Number, max :: Number, step :: [ discrete :: Number, continuous :: {} ] }
       , "Blue" :: { current :: Number, min :: Number, max :: Number, step :: [ discrete :: Number, continuous :: {} ] }
       )
      duskViolet

chipFace :: forall r1. { name :: String, mix :: { "Red" :: Number, "Green" :: Number, "Blue" :: Number } | r1 } -> String
chipFace { mix } = "width: 36px; height: 36px; border-radius: 50%; cursor: pointer; border: 1px solid #999; background-color: " <> rgb mix <> ";"

swatchStyle :: forall r1. { "Red" :: { current :: Number, min :: Number, max :: Number, step :: [ discrete :: Number, continuous :: {} ] }, "Green" :: { current :: Number, min :: Number, max :: Number, step :: [ discrete :: Number, continuous :: {} ] }, "Blue" :: { current :: Number, min :: Number, max :: Number, step :: [ discrete :: Number, continuous :: {} ] } | r1 } -> String
swatchStyle channels = "width: 100%; max-width: 420px; height: 120px; border-radius: 8px; border: 1px solid #ccc; background-color: " <> mixedColor channels <> ";"
