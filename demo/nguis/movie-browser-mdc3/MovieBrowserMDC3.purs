module MovieBrowserMDC3 (movieBrowserMDC3) where

import Prelude ((#), ($), Unit)

import Data.Variant (match)
import Effect (Effect)
import MovieBrowserViewModel (favoriteMark, favoritesLine, markFavorite, movieCatalogue, ratingLine, titleLine, visibleMovies, yearLine)
import PUI (foreach, mvu, state, toCase, updated)
import PUI.Web ((<+>), choice, shown, text)
import PUI.Web.HTML (span)
import PUI.Web.MDC3 (body, chipSet, elevation1, filterChip, iconToggle, list, listItem, titleMedium, tabBar)
import QualifiedDo.Semigroupoid as Semigroupoid

movieBrowserMDC3 :: Effect Unit
movieBrowserMDC3 =
  body $
    ( Semigroupoid.do
      state @"movies" @(Array { title :: String, year :: Int, category :: [ "All" :: {}, "Action" :: {}, "Drama" :: {}, "Comedy" :: {} ], tags :: Array [ "Classic" :: {}, "Cult" :: {}, "Oscar" :: {} ], rating :: Number, "Favorite" :: Boolean })
      tabBar @"category"
        (choice @"All" <+> choice @"Action" <+> choice @"Drama" <+> choice @"Comedy")
      chipSet ( Semigroupoid.do
        filterChip @"Classic" {}
        filterChip @"Cult" {}
        filterChip @"Oscar" {} )
      ( elevation1 $ titleMedium $ text favoritesLine ) # shown
      list $
        ( listItem $ Semigroupoid.do
          state @"year" @Int
          state @"rating" @Number
          span (text titleLine) # shown
          span (text yearLine) # shown
          span (text ratingLine) # shown
          iconToggle @"Favorite" { onIcon: "star", offIcon: "star_border" } ) # foreach @"title" @String visibleMovies # toCase @"favored" @{ title :: String, "Favorite" :: Boolean } favoriteMark # updated (match { favored: markFavorite })
    ) # mvu movieCatalogue
