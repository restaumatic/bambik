module MovieBrowserMDC3 (movieBrowserMDC3) where

import Prelude ((#), ($), Unit)

import Effect (Effect)
import MovieBrowserViewModel (favoriteMark, favoritesLine, markFavorite, movieCatalogue, ratingLine, titleLine, visibleMovies, yearLine)
import PUI (blankStatus, fold, foreach, joined, looped, toCase, with)
import PUI.Pipeline as Pipeline
import PUI.Web ((<+>), choice, text)
import PUI.Web.HTML (span)
import PUI.Web.MDC3 (body, chipSet, elevation1, filterChip, iconToggle, list, listItem, titleMedium, tabBar)

movieBrowserMDC3 :: Effect Unit
movieBrowserMDC3 =
  body $ Pipeline.do
    tabBar @"category"
      (choice @"All" <+> choice @"Action" <+> choice @"Drama" <+> choice @"Comedy")
    chipSet ( Pipeline.do
      filterChip @"Classic" {}
      filterChip @"Cult" {}
      filterChip @"Oscar" {} )
    elevation1 $ titleMedium $ text favoritesLine
    list $
      ( listItem $ Pipeline.do
        span (text titleLine)
        span (text yearLine)
        span (text ratingLine)
        iconToggle @"Favorite" { onIcon: "star", offIcon: "star_border" } ) # foreach @"title"
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
