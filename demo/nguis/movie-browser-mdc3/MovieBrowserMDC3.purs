module MovieBrowserMDC3 (movieBrowserMDC3) where

import Prelude (identity, (#), ($), Unit, show)

import Data.Variant (match)
import Effect (Effect)
import MovieBrowserLogic (favorites, markFavorite, movieCatalogue, ratingText, visibleMovies)
import PUI (asField, completed, displayed, foreach, forField, informed, mvu, projected, toCase, updated)
import PUI.Web.HTML (atCase, body, clWhen, span, staticText, text)
import PUI.Web.MDC3 (card, chipSet, elevation1, elevation3, filterChip, iconToggle, list, listItem, titleMedium, tabBar)
import QualifiedDo.Semigroupoid as Semigroupoid
import Data.Profunctor.Row.RecordToRecord as RecordToRecord

movieBrowserMDC3 :: Effect Unit
movieBrowserMDC3 =
  body $
    elevation3 $
      card { caption: "Movie Browser" } $ ( Semigroupoid.do
          tabBar
            [ { value: .all {}, label: "All" }
            , { value: .action {}, label: "Action" }
            , { value: .drama {}, label: "Drama" }
            , { value: .comedy {}, label: "Comedy" }
            ] # asField @"category" # completed
          chipSet ( RecordToRecord.do
              filterChip { label: "Classic" } # asField @"classic"
              filterChip { label: "Cult" } # asField @"cult"
              filterChip { label: "Oscar" } # asField @"oscar") # completed
          elevation1 ( titleMedium $ RecordToRecord.do
              text # forField @"count" show
              staticText " favorite" ) # atCase @"sole" favorites # displayed
          elevation1 ( titleMedium $ RecordToRecord.do
              text # forField @"count" show
              staticText " favorites" ) # atCase @"several" favorites # displayed
          list $
            ( clWhen _.favorite "mdc-deprecated-list-item--selected"
                $ listItem $ ( RecordToRecord.do
                    span text # forField @"title" identity
                    span text # forField @"year" show
                    span ( RecordToRecord.do
                        staticText "★ "
                        text # projected ratingText )
                    iconToggle { onIcon: "star", offIcon: "star_border", label: "Favorite" } # asField @"favorite") # completed) # foreach @"title" visibleMovies # toCase @"favored" identity # updated (match { favored: informed markFavorite })
      ) # mvu movieCatalogue
