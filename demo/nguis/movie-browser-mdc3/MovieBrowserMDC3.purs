module MovieBrowserMDC3 (movieBrowserMDC3) where

import Prelude ((#), ($), Unit)

import Data.Profunctor.Row.RecordToRecord as RecordToRecord
import Effect (Effect)
import MovieBrowserViewModel (favoriteMark, favoritesLine, markFavorite, movieCatalogue, ratingLine, titleLine, visibleMovies, yearLine)
import PUI (blankStatus, fold, foreach, joined, looped, prefixed, toCase, with)
import PUI.Web ((<+>), choice, text)
import PUI.Web.HTML (span)
import PUI.Web.MDC3 (body, chipSet, elevation1, filterChip, iconToggle, list, listItem, titleMedium, tabBar)
import QualifiedDo.Semigroupoid as Semigroupoid

movieBrowserMDC3 :: Effect Unit
movieBrowserMDC3 =
  body $ Semigroupoid.do
    tabBar @"category"
      (choice @"All" <+> choice @"Action" <+> choice @"Drama" <+> choice @"Comedy")
    chipSet ( Semigroupoid.do
      filterChip @"Classic" {}
      filterChip @"Cult" {}
      filterChip @"Oscar" {} )
    prefixed (elevation1 $ titleMedium $ text favoritesLine) $ list $
      ( listItem $ prefixed ( RecordToRecord.do
          span (text titleLine)
          span (text yearLine)
          span (text ratingLine) ) $ iconToggle @"Favorite" { onIcon: "star", offIcon: "star_border" } ) # foreach @"title"
          @( title :: String
           , year :: Int
           , rating :: Number
           , "Favorite" :: Boolean
           ) visibleMovies # toCase @"Favorite changed" @{ title :: String, "Favorite" :: Boolean } favoriteMark # joined @"Favorite changed"
    blankStatus @"Favorite changed" # fold markFavorite
  # looped
    @( category :: [ "All" :: {}, "Action" :: {}, "Drama" :: {}, "Comedy" :: {} ]
     , "Classic" :: Boolean
     , "Cult" :: Boolean
     , "Oscar" :: Boolean
     , movies :: Array { title :: String, year :: Int, category :: [ "All" :: {}, "Action" :: {}, "Drama" :: {}, "Comedy" :: {} ], tags :: Array [ "Classic" :: {}, "Cult" :: {}, "Oscar" :: {} ], rating :: Number, "Favorite" :: Boolean }
     ) # with movieCatalogue
