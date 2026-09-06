module PhotoGalleryMDC3 (photoGalleryMDC3) where

import Prelude ((#), ($), (<<<), Unit, const)

import Data.Profunctor.Row.RecordToRecord as RecordToRecord
import Data.Variant (match)
import Effect (Effect)
import PhotoGalleryLogic (albumChoices, albumPhotos, albumTitle, favoriteShots, isOpen, landscapesOpen, openAlbum)
import PUI (mvu, toCase, updated)
import PUI.Web.HTML (shownEach, shown, body, each, span, staticText, text)
import PUI.Web.MDC3 (displayMedium, divider, drawer, imageList, imageListItem, imagePane, labelSmall, list, listItem, listOf, topAppBar)
import QualifiedDo.Category as Category

photoGalleryMDC3 :: Effect Unit
photoGalleryMDC3 =
  body $
    topAppBar { title: "Photo Gallery" } $
      ( drawer { title: "Darkroom", subtitle: "photos drawn on the spot" }
          ( RecordToRecord.do
              listOf { selected: isOpen } albumChoices (span (text _.name)) # toCase @"albumPicked" _.name # updated (match { albumPicked: const <<< openAlbum })
              divider
              list RecordToRecord.do
                listItem $ staticText "Every photo is an SVG"
                listItem $ staticText "developed from its caption"
                listItem $ staticText "No network involved"
              labelSmall $ staticText "Favorites"
              imageList { columns: 2 } $ each favoriteShots imageListItem )
          ( Category.do
              ( displayMedium $ text albumTitle ) # shown
              imageList { columns: 3 } $ imagePane # shownEach @"src" albumPhotos )
      ) # mvu landscapesOpen
