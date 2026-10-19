module MovieBrowser.MDC3 (movieBrowser) where

import Prelude ((#), ($), Unit)

import Data.Profunctor.Row.VariantToVariant as VariantToVariant
import Effect (Effect)
import MovieBrowser (favoritesLine, listingLine, markFavorite, movieCatalogue, visibleMovies)
import PUI (anyEdit, fold, joined, looped, with)
import PUI.Web (text)
import PUI.Web.HTML (span)
import PUI.Web.MDC3 (body, chipSet, elevation1, filterChip, iconToggle, list, listItem, tab, tabBar, titleMedium)
import QualifiedDo.Semigroupoid as Semigroupoid

movieBrowser :: Effect Unit
movieBrowser =
  body $ Semigroupoid.do
    ( Semigroupoid.do
      tabBar @"Category" $ VariantToVariant.do
        tab @"All"
        tab @"Action"
        tab @"Drama"
        tab @"Comedy"
      chipSet $ Semigroupoid.do
        filterChip @"Classic"
        filterChip @"Cult"
        filterChip @"Oscar"
    ) # looped
    elevation1 $ titleMedium $ text favoritesLine
    list $
      ( listItem $ Semigroupoid.do
        span $ text listingLine
        iconToggle @"Favorite" { onIcon: "star", offIcon: "star_border" } ) # anyEdit @"Favorite changed" @"title" visibleMovies # joined @"Favorite changed"
    fold @"Favorite changed" markFavorite
  # looped # with movieCatalogue
