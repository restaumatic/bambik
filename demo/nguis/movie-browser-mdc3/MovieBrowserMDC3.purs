module MovieBrowserMDC3 (movieBrowserMDC3) where

import Prelude ((#), ($), Unit, show)

import Data.Variant (match)
import Effect (Effect)
import MovieBrowserLogic (favoriteMark, favorites, markFavorite, movieCatalogue, ratingText, visibleMovies)
import PUI (foreach, projection, mvu, projected, toCase, updated)
import PUI.Web (choice)
import PUI.Web.HTML (shown, shownCase, body, clWhen, span, staticText, text)
import PUI.Web.MDC3 (card, chipSet, elevation1, elevation3, filterChip, iconToggle, list, listItem, titleMedium, tabBar)
import QualifiedDo.Category as Category
import Data.Profunctor.Row.RecordToRecord as RecordToRecord

movieBrowserMDC3 :: Effect Unit
movieBrowserMDC3 =
  body $
    elevation3 $
      card $ ( Category.do
          tabBar @"category"
            [ choice @"All", choice @"Action", choice @"Drama", choice @"Comedy" ]
          chipSet ( Category.do
              filterChip @"Classic" {}
              filterChip @"Cult" {}
              filterChip @"Oscar" {})
          ( elevation1 $ titleMedium $ RecordToRecord.do
              text @"count" # projection show
              staticText " favorite" ) # shownCase @"sole" favorites
          ( elevation1 $ titleMedium $ RecordToRecord.do
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
