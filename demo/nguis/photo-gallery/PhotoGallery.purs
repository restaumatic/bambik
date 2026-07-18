module PhotoGallery (photoGallery) where

import Prelude ((#), ($), (*), (+), (<#>), (<>), (==), (>>>), Unit, mod, pure, show, unit)

import Data.Array (find, range)
import Data.Char (toCharCode)
import Data.Foldable (sum)
import Data.Maybe (maybe)
import Data.Profunctor (lcmap, rmap)
import Data.Profunctor.Row.RecordToRecord as RecordToRecord
import Data.String (joinWith)
import Data.String.CodeUnits (toCharArray)
import Data.Tuple (Tuple(..))
import Data.Variant (match)
import Effect (Effect)
import PUI (PUI, displayed, forValue, mvu, projection, tapped, updates)
import PUI.Web (Web)
import PUI.HTML (Markup(..), attr, body, div, foreach, span, staticText, text, view)
import PUI.MDC (divider, drawer, headline2, imageList, imageListItem, list, listItem, listOf, overline, topAppBar)
import QualifiedDo.Semigroupoid as Semigroupoid

photoGallery :: Effect Unit
photoGallery =
  body $
    topAppBar { title: "Photo Gallery" } $
      drawer { title: "Darkroom", subtitle: "photos drawn on the spot" }
        ( RecordToRecord.do
            list RecordToRecord.do
              listItem $ staticText "Every photo is an SVG"
              listItem $ staticText "developed from its caption"
              listItem $ staticText "No network involved"
            divider
            overline $ staticText "Favorites"
            imageList { columns: 2 } RecordToRecord.do
              imageListItem { src: developedPhoto "Dawn Ridge", label: "Dawn Ridge" }
              imageListItem { src: developedPhoto "Half Smile", label: "Half Smile" }
              imageListItem { src: developedPhoto "Orbit Study", label: "Orbit Study" }
              imageListItem { src: developedPhoto "Quiet Lake", label: "Quiet Lake" }
        )
        $ ( div >>> attr "style" "display: flex; gap: 24px; align-items: flex-start;" $ Semigroupoid.do
              ( attr "style" "flex: 0 0 180px; border-right: 1px solid #eee;"
                  ( listOf { selected: _.current } (span (text # projection _.name # forValue))
                  )
              ) # rmap (\a -> .albumPicked a.name :: [ albumPicked :: String ]) # lcmap albumChoices # updates (match { albumPicked: openAlbum })
              div >>> attr "style" "flex: 1; min-width: 0;" $ Semigroupoid.do
                headline2 (text # projection _.album # forValue) # tapped
                imageList { columns: 3 } (foreach photoTile) # lcmap albumPhotos # displayed
          ) # mvu landscapesOpen

photoTile :: PUI Web { src :: String, caption :: String } {}
photoTile =
  view """<li class="mdc-image-list__item" style="margin-bottom: 16px;"></li>"""
    ( \p ->
        [ Element "img" [ Tuple "class" "mdc-image-list__image", Tuple "src" p.src, Tuple "alt" p.caption ] []
        , Element "div" [ Tuple "class" "mdc-image-list__supporting" ]
            [ Element "span" [ Tuple "class" "mdc-image-list__label" ] [ Text p.caption ] ]
        ]
    )
    (\_ _ -> pure unit)

type Gallery = { album :: String }

landscapesOpen :: Gallery
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

albumChoices :: Gallery -> Array { name :: String, current :: Boolean }
albumChoices g = albumCatalogue <#> \a -> { name: a.name, current: a.name == g.album }

openAlbum :: String -> Gallery -> Gallery
openAlbum name g = g { album = name }

albumPhotos :: Gallery -> Array { src :: String, caption :: String }
albumPhotos g =
  maybe [] (\a -> a.shots <#> \caption -> { src: developedPhoto caption, caption })
    (find (\a -> a.name == g.album) albumCatalogue)

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
