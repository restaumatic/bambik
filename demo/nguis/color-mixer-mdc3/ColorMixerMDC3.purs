module ColorMixerMDC3 (colorMixerMDC3) where

import Prelude ((#), ($), (<>), (>>>), Unit, const)

import ColorMixerLogic (applyPreset, duskViolet, hexText, mixOf, palette, rgb, rgbText)
import Data.Variant (match)
import Effect (Effect)
import PUI (blank, foreach, mvu, projected, toCase, updated)
import PUI.Web.HTML (shown, attrWith, body, clicked, div, text, (:=))
import PUI.Web.MDC3 (bodyMedium, card, elevation5, sliderLive)
import QualifiedDo.Category as Category

colorMixerMDC3 :: Effect Unit
colorMixerMDC3 =
  body $
    elevation5 $
      card $ ( Category.do
          sliderLive @"Red" {}
          sliderLive @"Green" {}
          sliderLive @"Blue" {}
          ( div $ Category.do
              attrWith "style" swatchStyle $ div $ blank
              div >>> "style" := "display: flex; gap: 8px; margin-top: 10px;" $
                ( clicked ( div >>> attrWith "title" _.name >>> attrWith "style" chipFace $ blank ) ) # foreach @"name" (const palette)) # toCase @"preset" _.name # updated (match { preset: applyPreset })
          (bodyMedium (text @"hex" # projected hexText)) # shown
          (bodyMedium (text @"rgb" # projected rgbText)) # shown
      ) # mvu duskViolet
chipFace :: { name :: String, mix :: { "Red" :: Number, "Green" :: Number, "Blue" :: Number } } -> String
chipFace { mix } = chipStyle { mix }

chipStyle :: { mix :: { "Red" :: Number, "Green" :: Number, "Blue" :: Number } } -> String
chipStyle p = "width: 36px; height: 36px; border-radius: 50%; cursor: pointer; border: 1px solid #999; background-color: " <> rgb p.mix <> ";"

swatchStyle :: { "Red" :: { current :: Number, min :: Number, max :: Number, step :: [ discrete :: Number, continuous :: {} ] }, "Green" :: { current :: Number, min :: Number, max :: Number, step :: [ discrete :: Number, continuous :: {} ] }, "Blue" :: { current :: Number, min :: Number, max :: Number, step :: [ discrete :: Number, continuous :: {} ] } } -> String
swatchStyle channels = "width: 100%; max-width: 420px; height: 120px; border-radius: 8px; border: 1px solid #ccc; background-color: " <> rgb (mixOf channels) <> ";"
