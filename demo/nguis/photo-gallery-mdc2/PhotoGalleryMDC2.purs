module PhotoGalleryMDC2 (photoGalleryMDC2) where

import Prelude ((#), ($), Unit)

import Data.Profunctor.Row.RecordToRecord as RecordToRecord
import Data.Variant (match)
import Effect (Effect)
import PhotoGalleryViewModel (albumChoices, albumShots, albumTitle, developedShot, favoriteShots, isOpen, landscapesOpen, openAlbum)
import PUI (looped, updated, with)
import PUI.Web (each, shown, shownEach, staticText, text)
import PUI.Web.HTML (span)
import PUI.Web.MDC2 (body, divider, drawer, headline2, imageList, imageListItem, imagePane, list, listItem, listOf, overline, topAppBar)
import QualifiedDo.Semigroupoid as Semigroupoid

photoGalleryMDC2 :: Effect Unit
photoGalleryMDC2 =
  body $
    topAppBar @"Photo Gallery" $
      ( drawer @( title :: "Darkroom", subtitle :: "photos drawn on the spot" )
        ( Semigroupoid.do
          listOf @"opened" @"name" @( name :: String, state :: [ open :: {}, closed :: {} ] ) { selected: isOpen } albumChoices (span (text _.name)) # updated (match { opened: openAlbum })
          divider # shown
          ( list RecordToRecord.do
            listItem $ staticText "Every photo is an SVG"
            listItem $ staticText "developed from its caption"
            listItem $ staticText "No network involved" ) # shown
          ( overline $ staticText "Favorites" ) # shown
          ( imageList 2 $ each @{ src :: String, alt :: String } favoriteShots imageListItem ) # shown )
        ( Semigroupoid.do
          ( headline2 $ text albumTitle ) # shown
          imageList 3 $ imagePane developedShot # shownEach @"shot" @( shot :: String ) albumShots )
      ) # looped @( album :: String ) # with landscapesOpen
