module PhotoGalleryMDC3 (photoGalleryMDC3) where

import Prelude ((#), ($), Unit)

import Data.Profunctor.Row.RecordToRecord as RecordToRecord
import Data.Variant (match)
import Effect (Effect)
import PhotoGalleryLogic (albumChoices, albumShots, albumTitle, developedShot, favoriteShots, isOpen, landscapesOpen, openAlbum)
import PUI (mvu, updated)
import PUI.Web (each, shown, shownEach, staticText, text)
import PUI.Web.HTML (span)
import PUI.Web.MDC3 (body, displayMedium, divider, drawer, imageList, imageListItem, imagePane, labelSmall, list, listItem, listOf, topAppBar)
import QualifiedDo.Semigroupoid as Semigroupoid

photoGalleryMDC3 :: Effect Unit
photoGalleryMDC3 =
  body $
    topAppBar @"Photo Gallery" $
      ( drawer @( title :: "Darkroom", subtitle :: "photos drawn on the spot" )
        ( Semigroupoid.do
          listOf @"opened" @"name" { selected: isOpen } albumChoices (span (text _.name)) # updated (match { opened: openAlbum })
          divider # shown
          ( list RecordToRecord.do
            listItem $ staticText @"Every photo is an SVG"
            listItem $ staticText @"developed from its caption"
            listItem $ staticText @"No network involved" ) # shown
          ( labelSmall $ staticText @"Favorites" ) # shown
          ( imageList 2 $ each favoriteShots imageListItem ) # shown )
        ( Semigroupoid.do
          ( displayMedium $ text albumTitle ) # shown
          imageList 3 $ imagePane developedShot # shownEach @"shot" albumShots )
      ) # mvu landscapesOpen
