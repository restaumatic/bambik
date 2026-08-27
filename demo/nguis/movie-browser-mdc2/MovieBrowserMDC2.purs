module MovieBrowserMDC2 (movieBrowserMDC2) where

import Prelude ((#), ($), Unit, show)

import Data.Variant (match)
import Effect (Effect)
import MovieBrowserLogic (favoriteMark, favorites, markFavorite, movieCatalogue, ratingText, visibleMovies)
import PUI (foreach, projection, mvu, projected, toCase, updated)
import PUI.Web (choice)
import PUI.Web.HTML (shown, shownCase, body, clWhen, span, staticText, text)
import PUI.Web.MDC2 (card, chipSet, elevation1, elevation10, filterChip, iconToggle, list, listItem, subtitle1, tabBar)
import QualifiedDo.Category as Category
import Data.Profunctor.Row.RecordToRecord as RecordToRecord

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
          ( elevation1 $ subtitle1 $ RecordToRecord.do
              text @"count" # projection show
              staticText " favorite" ) # shownCase @"sole" favorites
          ( elevation1 $ subtitle1 $ RecordToRecord.do
              text @"count" # projection show
              staticText " favorites" ) # shownCase @"several" favorites
          list $
            ( clWhen _."Favorite" "mdc-deprecated-list-item--selected"
                $ listItem $ ( Category.do
                    ( RecordToRecord.do
                        span (text @"title")
                        span (text @"year") # projection show
                        span ( RecordToRecord.do
                            staticText "★ "
                            text @"rating" # projected ratingText ) ) # shown
                    iconToggle @"Favorite" { onIcon: "star", offIcon: "star_border" } ) ) # foreach @"title" visibleMovies # toCase @"favored" favoriteMark # updated (match { favored: markFavorite })
      ) # mvu movieCatalogue
