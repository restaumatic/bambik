module ColorMixerMDC3 (colorMixerMDC3) where

import Prelude ((#), ($), (<>), (>>>), Unit, const)

import ColorMixerLogic (applyPreset, duskViolet, hexText, mixOf, palette, rgb, rgbText)
import Data.Maybe (Maybe)
import Data.Variant (match)
import Effect (Effect)
import PUI (asField, completed, constantly, foreach, mvu, pempty, projected, tapped, toCase, updated)
import PUI.Web.HTML (attrWith, body, clicked, div, text, (:=))
import PUI.Web.MDC3 (bodyMedium, card, elevation5, sliderLive)
import QualifiedDo.Semigroupoid as Semigroupoid

colorMixerMDC3 :: Effect Unit
colorMixerMDC3 =
  body $
    elevation5 $
      card { caption: "Color Mixer" } $ ( Semigroupoid.do
          sliderLive { label: "Red" } # asField @"red" # completed
          sliderLive { label: "Green" } # asField @"green" # completed
          sliderLive { label: "Blue" } # asField @"blue" # completed
          ( div $ Semigroupoid.do
              attrWith "style" swatchStyle $ div $ pempty # constantly {}
              div >>> "style" := "display: flex; gap: 8px; margin-top: 10px;" $
                ( clicked ( div >>> attrWith "title" _.name >>> attrWith "style" (\p -> chipStyle { mix: p.mix }) $ pempty # constantly {} ) ) # foreach @"name" (const palette)) # toCase @"preset" _.name # updated (match { preset: applyPreset })
          bodyMedium text # projected hexText # tapped
          bodyMedium text # projected rgbText # tapped
      ) # mvu duskViolet

chipStyle :: { mix :: { red :: Number, green :: Number, blue :: Number } } -> String
chipStyle p = "width: 36px; height: 36px; border-radius: 50%; cursor: pointer; border: 1px solid #999; background-color: " <> rgb p.mix <> ";"

swatchStyle :: { red :: { current :: Number, min :: Number, max :: Number, step :: Maybe Number }, green :: { current :: Number, min :: Number, max :: Number, step :: Maybe Number }, blue :: { current :: Number, min :: Number, max :: Number, step :: Maybe Number } } -> String
swatchStyle channels = "width: 100%; max-width: 420px; height: 120px; border-radius: 8px; border: 1px solid #ccc; background-color: " <> rgb (mixOf channels) <> ";"
