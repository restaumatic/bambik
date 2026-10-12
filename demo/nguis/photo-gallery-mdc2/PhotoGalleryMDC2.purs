module PhotoGalleryMDC2 (photoGalleryMDC2) where

import Prelude ((#), ($), Unit)

import Data.Profunctor.Row.RecordToRecord as RecordToRecord
import Effect (Effect)
import PhotoGalleryViewModel (albumChoices, albumShots, albumTitle, developedShot, favoriteShots, isOpen, landscapesOpen, openAlbum)
import PUI (blankStatus, fold, foreach, joined, looped, suffixed, with)
import PUI.Web (each, staticText, text, textAt)
import PUI.Web.HTML (span)
import PUI.Web.MDC2 (body, divider, drawer, headline2, imageList, imageListItem, imagePane, list, listItem, listOf, overline, topAppBar)
import QualifiedDo.Semigroupoid as Semigroupoid

photoGalleryMDC2 :: Effect Unit
photoGalleryMDC2 =
  body $
    topAppBar "Photo Gallery" $
      ( drawer { title: "Darkroom", subtitle: "photos drawn on the spot" }
        ( Semigroupoid.do
          listOf @"opened" @"name" @( name :: String, state :: [ open :: {}, closed :: {} ] ) { selected: isOpen } albumChoices (span (textAt @"name")) # joined @"opened"
          blankStatus @"opened" # fold openAlbum # suffixed ( divider $ RecordToRecord.do
            ( list RecordToRecord.do
              listItem $ staticText "Every photo is an SVG"
              listItem $ staticText "developed from its caption"
              listItem $ staticText "No network involved" )
            overline $ staticText "Favorites"
            imageList 2 $ each @{ src :: String, alt :: String } favoriteShots imageListItem ) )
        ( RecordToRecord.do
          headline2 $ text albumTitle
          imageList 3 $ imagePane developedShot # foreach @"shot" @( shot :: String ) albumShots )
      ) # looped @( album :: String ) # with landscapesOpen
