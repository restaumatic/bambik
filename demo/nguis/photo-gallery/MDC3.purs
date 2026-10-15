module PhotoGallery.MDC3 (photoGallery) where

import Prelude ((#), ($), Unit)

import Effect (Effect)
import PhotoGallery (albumChoices, albumShots, developedShot, favoriteShots, isOpen, landscapesOpen, openAlbum)
import PUI (fold, foreach, joined, looped, with)
import PUI.Web (each, staticText, textAt)
import PUI.Web.MDC3 (body, displayMedium, divider, drawer, imageList, imageListItem, imagePane, labelSmall, list, listItem, listOf', topAppBar)
import QualifiedDo.Semigroupoid as Semigroupoid

photoGallery :: Effect Unit
photoGallery =
  body $
    topAppBar "Photo Gallery" $
      ( drawer { title: "Darkroom", subtitle: "photos drawn on the spot" }
        ( Semigroupoid.do
          ( listOf' @"opened" @"name" { selected: isOpen } albumChoices $ textAt @"name" ) # joined @"opened"
          fold @"opened" openAlbum
          divider $ Semigroupoid.do
            list $ Semigroupoid.do
              listItem $ staticText "Every photo is an SVG"
              listItem $ staticText "developed from its caption"
              listItem $ staticText "No network involved"
            labelSmall $ staticText "Favorites"
            imageList 2 $ each favoriteShots imageListItem )
        ( Semigroupoid.do
          displayMedium $ textAt @"album"
          imageList 3 $ imagePane developedShot # foreach @"shot" albumShots )
      ) # looped # with landscapesOpen
