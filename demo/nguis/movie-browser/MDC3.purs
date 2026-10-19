module MovieBrowser.MDC3 (movieBrowser) where

import Prelude ((#), ($), Unit)

import Data.Profunctor.Row.VariantToVariant as VariantToVariant
import Effect (Effect)
import MovieBrowser (favoritesLine, markFavorite, movieCatalogue, ratingLine, visibleMovies, yearLine)
import PUI (anyEdit, fold, joined, looped, with)
import PUI.Web (text, textAt)
import PUI.Web.HTML (span)
import PUI.Web.MDC3 (body, chipSet, elevation1, filterChip, iconToggle, list, listItem, tab, tabBar, titleMedium)
import QualifiedDo.Semigroupoid as Semigroupoid

movieBrowser :: Effect Unit
movieBrowser =
  body $ Semigroupoid.do
    tabBar @"category" $ VariantToVariant.do
      tab @"All"
      tab @"Action"
      tab @"Drama"
      tab @"Comedy"
    chipSet $ Semigroupoid.do
      filterChip @"Classic"
      filterChip @"Cult"
      filterChip @"Oscar"
    elevation1 $ titleMedium $ text favoritesLine
    list $
      ( listItem $ Semigroupoid.do
        span $ textAt @"title"
        span $ text yearLine
        span $ text ratingLine
        iconToggle @"Favorite" { onIcon: "star", offIcon: "star_border" } ) # anyEdit @"Favorite changed" @"title" visibleMovies # joined @"Favorite changed"
    fold @"Favorite changed" markFavorite
  # looped # with movieCatalogue
