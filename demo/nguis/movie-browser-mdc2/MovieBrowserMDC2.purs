module MovieBrowserMDC2 (movieBrowserMDC2) where

import Prelude (identity, (#), ($), Unit, show)

import Data.Variant (match)
import Effect (Effect)
import MovieBrowserLogic (favorites, markFavorite, movieCatalogue, ratingText, visibleMovies)
import PUI (completed, displayed, foreach, projection, informed, mvu, projected, toCase, updated)
import PUI.Web.HTML (providedCase, body, clWhen, span, staticText, text)
import PUI.Web.MDC2 (card, chipSet, elevation1, elevation10, filterChip, iconToggle, list, listItem, subtitle1, tabBar)
import QualifiedDo.Semigroupoid as Semigroupoid
import Data.Profunctor.Row.RecordToRecord as RecordToRecord

movieBrowserMDC2 :: Effect Unit
movieBrowserMDC2 =
  body $
    elevation10 $
      card { caption: "Movie Browser" } $ ( Semigroupoid.do
          tabBar @"category"
            [ { value: .all {}, label: "All" }
            , { value: .action {}, label: "Action" }
            , { value: .drama {}, label: "Drama" }
            , { value: .comedy {}, label: "Comedy" }
            ] # completed
          chipSet ( RecordToRecord.do
              filterChip @"Classic" {}
              filterChip @"Cult" {}
              filterChip @"Oscar" {}) # completed
          elevation1 ( subtitle1 $ RecordToRecord.do
              text @"count" # projection show
              staticText " favorite" ) # providedCase @"sole" favorites # displayed
          elevation1 ( subtitle1 $ RecordToRecord.do
              text @"count" # projection show
              staticText " favorites" ) # providedCase @"several" favorites # displayed
          list $
            ( clWhen _."Favorite" "mdc-deprecated-list-item--selected"
                $ listItem $ ( RecordToRecord.do
                    span (text @"title")
                    span (text @"year") # projection show
                    span ( RecordToRecord.do
                        staticText "★ "
                        text @"rating" # projected ratingText )
                    iconToggle @"Favorite" { onIcon: "star", offIcon: "star_border" }) # completed) # foreach @"title" visibleMovies # toCase @"favored" identity # updated (match { favored: informed markFavorite })
      ) # mvu movieCatalogue
