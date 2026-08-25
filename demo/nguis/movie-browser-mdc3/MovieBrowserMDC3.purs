module MovieBrowserMDC3 (movieBrowserMDC3) where

import Prelude (identity, (#), ($), Unit, show)

import Data.Variant (match)
import Effect (Effect)
import MovieBrowserLogic (favorites, markFavorite, movieCatalogue, ratingText, visibleMovies)
import PUI (foreach, projection, informed, mvu, projected, toCase, updated)
import PUI.Web (choice)
import PUI.Web.HTML (shownAs, shownCase, body, clWhen, span, staticText, text)
import PUI.Web.MDC3 (card, chipSet, elevation1, elevation3, filterChip, iconToggle, list, listItem, titleMedium, tabBar)
import QualifiedDo.Semigroupoid as Semigroupoid
import Data.Profunctor.Row.RecordToRecord as RecordToRecord

movieBrowserMDC3 :: Effect Unit
movieBrowserMDC3 =
  body $
    elevation3 $
      card $ ( Semigroupoid.do
          tabBar @"category"
            [ choice @"All", choice @"Action", choice @"Drama", choice @"Comedy" ]
          chipSet ( Semigroupoid.do
              filterChip @"Classic" {}
              filterChip @"Cult" {}
              filterChip @"Oscar" {})
          shownCase @"sole" favorites ( elevation1 $ titleMedium $ RecordToRecord.do
              text @"count" # projection show
              staticText " favorite" )
          shownCase @"several" favorites ( elevation1 $ titleMedium $ RecordToRecord.do
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
