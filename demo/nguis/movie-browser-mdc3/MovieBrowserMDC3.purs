module MovieBrowserMDC3 (movieBrowserMDC3) where

import Prelude ((#), ($), Unit)

import Data.Variant (match)
import Effect (Effect)
import MovieBrowserLogic (favoriteMark, favoritesLine, markFavorite, movieCatalogue, ratingLine, titleLine, visibleMovies, yearLine)
import PUI (foreach, mvu, toCase, updated)
import PUI.Web ((<+>), choice, shown, text)
import PUI.Web.HTML (span)
import PUI.Web.MDC3 (body, chipSet, elevation1, filterChip, iconToggle, list, listItem, titleMedium, tabBar)
import QualifiedDo.Semigroupoid as Semigroupoid

movieBrowserMDC3 :: Effect Unit
movieBrowserMDC3 =
  body $
    ( Semigroupoid.do
      tabBar @"category"
        (choice @"All" <+> choice @"Action" <+> choice @"Drama" <+> choice @"Comedy")
      chipSet ( Semigroupoid.do
        filterChip @"Classic" {}
        filterChip @"Cult" {}
        filterChip @"Oscar" {} )
      ( elevation1 $ titleMedium $ text favoritesLine ) # shown
      list $
        ( listItem $ Semigroupoid.do
          span (text titleLine) # shown
          span (text yearLine) # shown
          span (text ratingLine) # shown
          iconToggle @"Favorite" { onIcon: "star", offIcon: "star_border" } ) # foreach @"title" visibleMovies # toCase @"favored" favoriteMark # updated (match { favored: markFavorite })
    ) # mvu movieCatalogue
