module MovieBrowser.MDC2 (movieBrowser) where

import Prelude ((#), ($), Unit)

import Effect (Effect)
import MovieBrowser (favoriteMark, favoritesLine, isFavorite, markFavorite, movieCatalogue, ratingLine, titleLine, visibleMovies, yearLine)
import PUI (anyOf, fold, joined, looped, toCase, unshown, with)
import PUI.Web ((<+>), choice, clWhen, text)
import PUI.Web.HTML (span)
import PUI.Web.MDC2 (body, chipSet, elevation1, filterChip, iconToggle, list, listItem, subtitle1, tabBar)
import QualifiedDo.Semigroupoid as Semigroupoid

movieBrowser :: Effect Unit
movieBrowser =
  body $ Semigroupoid.do
    tabBar @"category"
      (choice @"All" <+> choice @"Action" <+> choice @"Drama" <+> choice @"Comedy")
    chipSet ( Semigroupoid.do
      filterChip @"Classic"
      filterChip @"Cult"
      filterChip @"Oscar" )
    elevation1 $ subtitle1 $ text favoritesLine
    list $
      ( listItem $ Semigroupoid.do
        span (text titleLine)
        span (text yearLine)
        span (text ratingLine)
        iconToggle @"Favorite" { onIcon: "star", offIcon: "star_border" } ) # clWhen isFavorite "mdc-deprecated-list-item--selected" # toCase @"Favorite changed" @{ title :: String, "Favorite" :: Boolean } favoriteMark # anyOf @"title"
        @( title :: String
         , year :: Int
         , rating :: Number
         , "Favorite" :: Boolean
         ) visibleMovies # joined @"Favorite changed"
    unshown @"Favorite changed" # fold markFavorite
  # looped
    @( category :: [ "All" :: {}, "Action" :: {}, "Drama" :: {}, "Comedy" :: {} ]
     , "Classic" :: Boolean
     , "Cult" :: Boolean
     , "Oscar" :: Boolean
     , movies :: Array { title :: String, year :: Int, category :: [ "All" :: {}, "Action" :: {}, "Drama" :: {}, "Comedy" :: {} ], tags :: Array [ "Classic" :: {}, "Cult" :: {}, "Oscar" :: {} ], rating :: Number, "Favorite" :: Boolean }
     ) # with movieCatalogue
