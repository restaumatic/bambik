module MovieBrowser.MDC2 (movieBrowser) where

import Prelude ((#), ($), (>>>), Unit)

import Data.Profunctor.Row.VariantToVariant as VariantToVariant
import Effect (Effect)
import MovieBrowser (favoritesLine, isFavorite, listingLine, movieCatalogue, visibleMovies)
import PUI (actedWithin, looped, with)
import PUI.Web (clWhen, text)
import PUI.Web.HTML (span)
import PUI.Web.MDC2 (body, chipSet, elevation1, filterChip, iconToggle, list, listItem, subtitle1, tab, tabBar)
import QualifiedDo.Semigroupoid as Semigroupoid

movieBrowser :: Effect Unit
movieBrowser =
  body $ Semigroupoid.do
    tabBar @"Category" $ VariantToVariant.do
      tab @"All"
      tab @"Action"
      tab @"Drama"
      tab @"Comedy"
    chipSet $ Semigroupoid.do
      filterChip @"Classic"
      filterChip @"Cult"
      filterChip @"Oscar"
    elevation1 $ subtitle1 $ text favoritesLine
    list $
      ( listItem >>> clWhen isFavorite "mdc-deprecated-list-item--selected" $ Semigroupoid.do
        span $ text listingLine
        iconToggle @"Favorite" { onIcon: "star", offIcon: "star_border" } ) # actedWithin @"title" @"movies" visibleMovies
  # looped # with movieCatalogue
