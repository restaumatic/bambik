module MovieBrowserMDC2 (movieBrowserMDC2) where

import Prelude ((#), ($), Unit)

import Data.Profunctor.Row.RecordToRecord as RecordToRecord
import Effect (Effect)
import MovieBrowserViewModel (favoriteMark, favoritesLine, isFavorite, markFavorite, movieCatalogue, ratingLine, titleLine, visibleMovies, yearLine)
import PUI (fold, foreach, joined, looped, prefixed, toCase, unshown, with)
import PUI.Web ((<+>), choice, clWhen, text)
import PUI.Web.HTML (span)
import PUI.Web.MDC2 (body, chipSet, elevation1, filterChip, iconToggle, list, listItem, subtitle1, tabBar)
import QualifiedDo.Semigroupoid as Semigroupoid

movieBrowserMDC2 :: Effect Unit
movieBrowserMDC2 =
  body $ Semigroupoid.do
    tabBar @"category"
      (choice @"All" <+> choice @"Action" <+> choice @"Drama" <+> choice @"Comedy")
    chipSet ( Semigroupoid.do
      filterChip @"Classic"
      filterChip @"Cult"
      filterChip @"Oscar" )
    prefixed (elevation1 $ subtitle1 $ text favoritesLine) $ list $
      ( listItem $ prefixed ( RecordToRecord.do
        span (text titleLine)
        span (text yearLine)
        span (text ratingLine) ) $ iconToggle @"Favorite" { onIcon: "star", offIcon: "star_border" } ) # clWhen isFavorite "mdc-deprecated-list-item--selected" # foreach @"title"
        @( title :: String
         , year :: Int
         , rating :: Number
         , "Favorite" :: Boolean
         ) visibleMovies # toCase @"Favorite changed" @{ title :: String, "Favorite" :: Boolean } favoriteMark # joined @"Favorite changed"
    unshown @"Favorite changed" # fold markFavorite
  # looped
    @( category :: [ "All" :: {}, "Action" :: {}, "Drama" :: {}, "Comedy" :: {} ]
     , "Classic" :: Boolean
     , "Cult" :: Boolean
     , "Oscar" :: Boolean
     , movies :: Array { title :: String, year :: Int, category :: [ "All" :: {}, "Action" :: {}, "Drama" :: {}, "Comedy" :: {} ], tags :: Array [ "Classic" :: {}, "Cult" :: {}, "Oscar" :: {} ], rating :: Number, "Favorite" :: Boolean }
     ) # with movieCatalogue
