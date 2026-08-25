module MovieBrowserMDC2 (movieBrowserMDC2) where

import Prelude (identity, (#), ($), Unit, show)

import Data.Variant (match)
import Effect (Effect)
import MovieBrowserLogic (favorites, markFavorite, movieCatalogue, ratingText, visibleMovies)
import PUI (foreach, projection, informed, mvu, projected, toCase, updated)
import PUI.Web (choice)
import PUI.Web.HTML (shownAs, shownCase, body, clWhen, span, staticText, text)
import PUI.Web.MDC2 (card, chipSet, elevation1, elevation10, filterChip, iconToggle, list, listItem, subtitle1, tabBar)
import QualifiedDo.Semigroupoid as Semigroupoid
import Data.Profunctor.Row.RecordToRecord as RecordToRecord

movieBrowserMDC2 :: Effect Unit
movieBrowserMDC2 =
  body $
    elevation10 $
      card $ ( Semigroupoid.do
          tabBar @"category"
            [ choice @"All", choice @"Action", choice @"Drama", choice @"Comedy" ]
          chipSet ( Semigroupoid.do
              filterChip @"Classic" {}
              filterChip @"Cult" {}
              filterChip @"Oscar" {})
          shownCase @"sole" favorites ( elevation1 $ subtitle1 $ RecordToRecord.do
              text @"count" # projection show
              staticText " favorite" )
          shownCase @"several" favorites ( elevation1 $ subtitle1 $ RecordToRecord.do
              text @"count" # projection show
              staticText " favorites" )
          list $
            ( clWhen _."Favorite" "mdc-deprecated-list-item--selected"
                $ listItem $ ( Semigroupoid.do
                    shownAs identity ( RecordToRecord.do
                        span (text @"title")
                        span (text @"year") # projection show
                        span ( RecordToRecord.do
                            staticText "★ "
                            text @"rating" # projected ratingText ) )
                    iconToggle @"Favorite" { onIcon: "star", offIcon: "star_border" } ) ) # foreach @"title" visibleMovies # toCase @"favored" identity # updated (match { favored: informed markFavorite })
      ) # mvu movieCatalogue
