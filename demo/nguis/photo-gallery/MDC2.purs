module PhotoGallery.MDC2 (photoGallery) where

import Prelude ((#), ($), Unit)

import Effect (Effect)
import PhotoGallery (albumChoices, albumShots, developedShot, favoriteShots, isOpen, landscapesOpen, openAlbum)
import PUI (actedOf, foldInto, looped, stepped, with)
import PUI.Web (each, staticText, textAt)
import PUI.Web.MDC2 (body, divider, drawer, headline2, imageList, imageListItem, imagePane, list, listItem, listOf', overline, topAppBar)
import QualifiedDo.Semigroupoid as Semigroupoid

photoGallery :: Effect Unit
photoGallery =
  body $
    topAppBar "Photo Gallery" $
      ( drawer { title: "Darkroom", subtitle: "photos drawn on the spot" }
        ( Semigroupoid.do
          ( Semigroupoid.do
            listOf' @"Album opened" @"name" { selected: isOpen } albumChoices $ textAt @"name"
            foldInto @"Album opened" openAlbum ) # stepped
          divider $ Semigroupoid.do
            list $ Semigroupoid.do
              listItem $ staticText "Every photo is an SVG developed from its caption"
              listItem $ staticText "No network involved"
            overline $ staticText "Favorites"
            imageList 2 $ each favoriteShots imageListItem )
        ( Semigroupoid.do
          headline2 $ textAt @"album"
          imageList 3 $ imagePane developedShot # actedOf @"shot" albumShots )
      ) # looped # with landscapesOpen
