module PhotoGallery.MDC3 (photoGallery) where

import Prelude ((#), ($), Unit)

import Effect (Effect)
import PhotoGallery (albumChoices, albumShots, albumTitle, developedShot, favoriteShots, isOpen, landscapesOpen, openAlbum)
import PUI (fold, foreach, joined, looped, with)
import PUI.Web (each, staticText, text, textAt)
import PUI.Web.HTML (span)
import PUI.Web.MDC3 (body, displayMedium, divider, drawer, imageList, imageListItem, imagePane, labelSmall, list, listItem, listOf', topAppBar)
import QualifiedDo.Semigroupoid as Semigroupoid

photoGallery :: Effect Unit
photoGallery =
  body $
    topAppBar "Photo Gallery" $
      ( drawer { title: "Darkroom", subtitle: "photos drawn on the spot" }
        ( Semigroupoid.do
          ( listOf' @"opened" @"name" @( name :: String, state :: [ open :: {}, closed :: {} ] ) { selected: isOpen } albumChoices $ span $ textAt @"name" ) # joined
          fold @"opened" openAlbum
          divider $ Semigroupoid.do
            ( list Semigroupoid.do
              listItem $ staticText "Every photo is an SVG"
              listItem $ staticText "developed from its caption"
              listItem $ staticText "No network involved" )
            labelSmall $ staticText "Favorites"
            imageList 2 $ each @{ src :: String, alt :: String } favoriteShots imageListItem )
        ( Semigroupoid.do
          displayMedium $ text albumTitle
          imageList 3 $ imagePane developedShot # foreach @"shot" @( shot :: String ) albumShots )
      ) # looped @( album :: String ) # with landscapesOpen
