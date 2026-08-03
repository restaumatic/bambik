module PhotoGalleryLogic (albumChoices, albumPhotos, developedPhoto, landscapesOpen, openAlbum) where

import Prelude (($), (*), (+), (<#>), (<>), (==), mod, show)

import Data.Array (find, range)
import Data.Char (toCharCode)
import Data.Foldable (sum)
import Data.Maybe (maybe)
import Data.String (joinWith)
import Data.String.CodeUnits (toCharArray)

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

openAlbum :: String -> { album :: String }
openAlbum album = { album }

albumPhotos :: { album :: String } -> Array { src :: String, label :: String }
albumPhotos { album } =
  maybe [] (\a -> a.shots <#> \label -> { src: developedPhoto label, label })
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
