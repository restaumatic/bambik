module MovieBrowser.MDC2 (movieBrowser) where

import Prelude ((#), ($), (>>>), Unit)

import Effect (Effect)
import MovieBrowser (favoritesLine, isFavorite, markFavorite, movieCatalogue, ratingLine, visibleMovies, yearLine)
import PUI (anyEdit, fold, joined, looped, with)
import PUI.Web ((<+>), choice, clWhen, text, textAt)
import PUI.Web.HTML (span)
import PUI.Web.MDC2 (body, chipSet, elevation1, filterChip, iconToggle, list, listItem, subtitle1, tabBar)
import QualifiedDo.Semigroupoid as Semigroupoid

movieBrowser :: Effect Unit
movieBrowser =
  body $ Semigroupoid.do
    tabBar @"category"
      (choice @"All" <+> choice @"Action" <+> choice @"Drama" <+> choice @"Comedy")
    chipSet $ Semigroupoid.do
      filterChip @"Classic"
      filterChip @"Cult"
      filterChip @"Oscar"
    elevation1 $ subtitle1 $ text favoritesLine
    list $
      ( listItem >>> clWhen isFavorite "mdc-deprecated-list-item--selected" $ Semigroupoid.do
        span $ textAt @"title"
        span $ text yearLine
        span $ text ratingLine
        iconToggle @"Favorite" { onIcon: "star", offIcon: "star_border" } ) # anyEdit @"Favorite changed" @"title" visibleMovies # joined @"Favorite changed"
    fold @"Favorite changed" markFavorite
  # looped # with movieCatalogue
