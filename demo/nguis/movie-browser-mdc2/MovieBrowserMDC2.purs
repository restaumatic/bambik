module MovieBrowserMDC2 (movieBrowserMDC2) where

import Prelude (identity, (#), ($), Unit, show)

import Data.Variant (match)
import Effect (Effect)
import MovieBrowserLogic (favorites, markFavorite, movieCatalogue, ratingText, visibleMovies)
import PUI (asField, completed, displayed, foreach, forField, informed, mvu, projected, toCase, updated)
import PUI.Web.HTML (providedCase, body, clWhen, span, staticText, text)
import PUI.Web.MDC2 (card, chipSet, elevation1, elevation10, filterChip, iconToggle, list, listItem, subtitle1, tabBar)
import QualifiedDo.Semigroupoid as Semigroupoid
import Data.Profunctor.Row.RecordToRecord as RecordToRecord

movieBrowserMDC2 :: Effect Unit
movieBrowserMDC2 =
  body $
    elevation10 $
      card { caption: "Movie Browser" } $ ( Semigroupoid.do
          tabBar
            [ { value: .all {}, label: "All" }
            , { value: .action {}, label: "Action" }
            , { value: .drama {}, label: "Drama" }
            , { value: .comedy {}, label: "Comedy" }
            ] # asField @"value" @"category" # completed
          chipSet ( RecordToRecord.do
              filterChip { label: "Classic" } # asField @"value" @"classic"
              filterChip { label: "Cult" } # asField @"value" @"cult"
              filterChip { label: "Oscar" } # asField @"value" @"oscar") # completed
          elevation1 ( subtitle1 $ RecordToRecord.do
              text # forField @"count" show
              staticText " favorite" ) # providedCase @"sole" favorites # displayed
          elevation1 ( subtitle1 $ RecordToRecord.do
              text # forField @"count" show
              staticText " favorites" ) # providedCase @"several" favorites # displayed
          list $
            ( clWhen _.favorite "mdc-deprecated-list-item--selected"
                $ listItem $ ( RecordToRecord.do
                    span text # forField @"title" identity
                    span text # forField @"year" show
                    span ( RecordToRecord.do
                        staticText "★ "
                        text # projected @"value" ratingText )
                    iconToggle { onIcon: "star", offIcon: "star_border", label: "Favorite" } # asField @"value" @"favorite") # completed) # foreach @"title" visibleMovies # toCase @"favored" identity # updated (match { favored: informed markFavorite })
      ) # mvu movieCatalogue
