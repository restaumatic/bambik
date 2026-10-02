module MovieBrowserMDC2 (movieBrowserMDC2) where

import Prelude ((#), ($), Unit)

import Data.Variant (match)
import Effect (Effect)
import MovieBrowserViewModel (favoriteMark, favoritesLine, isFavorite, markFavorite, movieCatalogue, ratingLine, titleLine, visibleMovies, yearLine)
import PUI (foreach, mvu, state, toCase, updated)
import PUI.Web ((<+>), choice, clWhen, shown, text)
import PUI.Web.HTML (span)
import PUI.Web.MDC2 (body, chipSet, elevation1, filterChip, iconToggle, list, listItem, subtitle1, tabBar)
import QualifiedDo.Semigroupoid as Semigroupoid

movieBrowserMDC2 :: Effect Unit
movieBrowserMDC2 =
  body $
    ( Semigroupoid.do
      state @"movies" @(Array { title :: String, year :: Int, category :: [ "All" :: {}, "Action" :: {}, "Drama" :: {}, "Comedy" :: {} ], tags :: Array [ "Classic" :: {}, "Cult" :: {}, "Oscar" :: {} ], rating :: Number, "Favorite" :: Boolean })
      tabBar @"category"
        (choice @"All" <+> choice @"Action" <+> choice @"Drama" <+> choice @"Comedy")
      chipSet ( Semigroupoid.do
        filterChip @"Classic" {}
        filterChip @"Cult" {}
        filterChip @"Oscar" {} )
      ( elevation1 $ subtitle1 $ text favoritesLine ) # shown
      list $
        ( listItem $ Semigroupoid.do
          state @"year" @Int
          state @"rating" @Number
          span (text titleLine) # shown
          span (text yearLine) # shown
          span (text ratingLine) # shown
          iconToggle @"Favorite" { onIcon: "star", offIcon: "star_border" } ) # clWhen isFavorite "mdc-deprecated-list-item--selected" # foreach @"title" @String visibleMovies # toCase @"favored" @{ title :: String, "Favorite" :: Boolean } favoriteMark # updated (match { favored: markFavorite })
    ) # mvu movieCatalogue
