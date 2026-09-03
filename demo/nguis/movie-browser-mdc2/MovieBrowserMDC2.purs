module MovieBrowserMDC2 (movieBrowserMDC2) where

import Prelude ((#), ($), Unit)

import Data.Variant (match)
import Effect (Effect)
import MovieBrowserLogic (favoriteMark, favorites, favoritesLine, markFavorite, movieCatalogue, ratingLine, titleLine, visibleMovies, yearLine)
import PUI (foreach, mvu, toCase, updated)
import PUI.Web (choice)
import PUI.Web.HTML (shown, shownWhen, body, clWhen, span, text)
import PUI.Web.MDC2 (card, chipSet, elevation1, elevation10, filterChip, iconToggle, list, listItem, subtitle1, tabBar)
import QualifiedDo.Category as Category

movieBrowserMDC2 :: Effect Unit
movieBrowserMDC2 =
  body $
    elevation10 $
      card $ ( Category.do
          tabBar @"category"
            [ choice @"All", choice @"Action", choice @"Drama", choice @"Comedy" ]
          chipSet ( Category.do
              filterChip @"Classic" {}
              filterChip @"Cult" {}
              filterChip @"Oscar" {} )
          ( elevation1 $ subtitle1 $ text favoritesLine ) # shownWhen @"sole" favorites
          ( elevation1 $ subtitle1 $ text favoritesLine ) # shownWhen @"several" favorites
          list $
            ( clWhen _."Favorite" "mdc-deprecated-list-item--selected"
                $ listItem $ ( Category.do
                    span (text titleLine) # shown
                    span (text yearLine) # shown
                    span (text ratingLine) # shown
                    iconToggle @"Favorite" { onIcon: "star", offIcon: "star_border" } ) ) # foreach @"title" visibleMovies # toCase @"favored" favoriteMark # updated (match { favored: markFavorite })
      ) # mvu movieCatalogue
