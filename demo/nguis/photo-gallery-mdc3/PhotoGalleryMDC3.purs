module PhotoGalleryMDC3 (photoGalleryMDC3) where

import Prelude ((#), ($), Unit)

import Data.Profunctor.Row.RecordToRecord as RecordToRecord
import Effect (Effect)
import PhotoGalleryViewModel (albumChoices, albumShots, albumTitle, developedShot, favoriteShots, isOpen, landscapesOpen, openAlbum)
import PUI (fold, foreach, joined, looped, suffixed, unshown, with)
import PUI.Web (each, staticText, text, textAt)
import PUI.Web.HTML (span)
import PUI.Web.MDC3 (body, displayMedium, divider, drawer, imageList, imageListItem, imagePane, labelSmall, list, listItem, listOfWith, topAppBar)
import QualifiedDo.Semigroupoid as Semigroupoid

photoGalleryMDC3 :: Effect Unit
photoGalleryMDC3 =
  body $
    topAppBar "Photo Gallery" $
      ( drawer { title: "Darkroom", subtitle: "photos drawn on the spot" }
        ( Semigroupoid.do
          listOfWith @"opened" @"name" @( name :: String, state :: [ open :: {}, closed :: {} ] ) { selected: isOpen } albumChoices (span (textAt @"name")) # joined @"opened"
          unshown @"opened" # fold openAlbum # suffixed ( divider $ RecordToRecord.do
            ( list RecordToRecord.do
              listItem $ staticText "Every photo is an SVG"
              listItem $ staticText "developed from its caption"
              listItem $ staticText "No network involved" )
            labelSmall $ staticText "Favorites"
            imageList 2 $ each @{ src :: String, alt :: String } favoriteShots imageListItem ) )
        ( RecordToRecord.do
          displayMedium $ text albumTitle
          imageList 3 $ imagePane developedShot # foreach @"shot" @( shot :: String ) albumShots )
      ) # looped @( album :: String ) # with landscapesOpen
