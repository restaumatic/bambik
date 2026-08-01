module PhotoGalleryMDC2 (photoGalleryMDC2) where

import Prelude ((#), ($), (*), (+), (<#>), (<>), (==), Unit, mod, show)

import Data.Array (find, range)
import Data.Char (toCharCode)
import Data.Foldable (sum)
import Data.Maybe (maybe)
import Data.Profunctor.Row.RecordToRecord as RecordToRecord
import Data.String (joinWith)
import Data.String.CodeUnits (toCharArray)
import Data.Variant (match)
import Effect (Effect)
import PUI (displayed, forField, forValue, mvu, ofField, tapped, toCase, updates)
import PUI.HTML (body, dynamic, each, span, staticText, text)
import PUI.MDC2 (divider, drawer, headline2, imageList, imageListItem, list, listItem, listOf, overline, topAppBar)
import QualifiedDo.Semigroupoid as Semigroupoid

photoGalleryMDC2 :: Effect Unit
photoGalleryMDC2 =
  body $
    topAppBar { title: "Photo Gallery" } $
      ( drawer { title: "Darkroom", subtitle: "photos drawn on the spot" }
          ( RecordToRecord.do
              listOf { selected: _.current } albumChoices (span text # ofField @"name") # toCase @"albumPicked" _.name # updates (match { albumPicked: openAlbum })
              divider
              list RecordToRecord.do
                listItem $ staticText "Every photo is an SVG"
                listItem $ staticText "developed from its caption"
                listItem $ staticText "No network involved"
              overline $ staticText "Favorites"
              imageList { columns: 2 } RecordToRecord.do
                imageListItem { src: developedPhoto "Dawn Ridge", label: "Dawn Ridge" }
                imageListItem { src: developedPhoto "Half Smile", label: "Half Smile" }
                imageListItem { src: developedPhoto "Orbit Study", label: "Orbit Study" }
                imageListItem { src: developedPhoto "Quiet Lake", label: "Quiet Lake" })
          ( Semigroupoid.do
              headline2 text # forValue # forField @"album" # tapped
              imageList { columns: 3 } $ displayed $ dynamic \m ->
                each (albumPhotos m) \p -> imageListItem { src: p.src, label: p.caption })
      ) # mvu landscapesOpen

landscapesOpen :: { album :: String }
landscapesOpen = { album: "Landscapes" }

albumCatalogue :: Array { name :: String, shots :: Array String }
albumCatalogue =
  [ { name: "Landscapes"
    , shots:
        [ "Dawn Ridge", "Quiet Lake", "Amber Dunes", "Foggy Pass", "Birch Line"
        , "Tidal Flats", "Storm Front", "Green Valley", "Last Light", "Winter Field"
        ]
    }
  , { name: "Portraits"
    , shots:
        [ "Half Smile", "Sunday Hat", "The Violinist", "Grandfather", "Sideways Glance"
        , "Freckles", "After the Match", "Reader by the Window"
        ]
    }
  , { name: "Abstract"
    , shots:
        [ "Orbit Study", "Noise Floor", "Copper Wash", "Interference", "Split Tone"
        , "Modulation", "Phase Shift", "Grain Field", "Vector Bloom", "Slow Collapse"
        , "Residue", "Afterimage"
        ]
    }
  ]

albumChoices :: { album :: String } -> Array { name :: String, current :: Boolean }
albumChoices { album } = albumCatalogue <#> \a -> { name: a.name, current: a.name == album }

openAlbum :: String -> { album :: String } -> { album :: String }
openAlbum name g = g { album = name }

albumPhotos :: { album :: String } -> Array { src :: String, caption :: String }
albumPhotos { album } =
  maybe [] (\a -> a.shots <#> \caption -> { src: developedPhoto caption, caption })
    (find (\a -> a.name == album) albumCatalogue)

developedPhoto :: String -> String
developedPhoto caption =
  "data:image/svg+xml;utf8,<svg xmlns='http://www.w3.org/2000/svg' width='320' height='" <> show height
    <> "' viewBox='0 0 320 " <> show height <> "'>"
    <> "<rect width='100%25' height='100%25' fill='hsl(" <> show hue <> ",45%25,78%25)'/>"
    <> shapes
    <> "</svg>"
  where
  grain = sum (toCharArray caption <#> toCharCode)
  hue = (grain * 7) `mod` 360
  height = 160 + (grain `mod` 5) * 40
  shapes = joinWith "" $ range 0 (2 + grain `mod` 3) <#> \i ->
    "<circle cx='" <> show ((grain * (i + 3) * 37) `mod` 320)
      <> "' cy='" <> show ((grain * (i + 5) * 53) `mod` height)
      <> "' r='" <> show (18 + (grain * (i + 2)) `mod` 42)
      <> "' fill='hsl(" <> show ((hue + 40 + i * 55) `mod` 360) <> ",55%25,50%25)' fill-opacity='0.7'/>"
