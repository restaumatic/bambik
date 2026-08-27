module PhotoGalleryMDC2 (photoGalleryMDC2) where

import Prelude ((#), ($), (<<<), Unit, const)

import Data.Profunctor.Row.RecordToRecord as RecordToRecord
import Data.Variant (match)
import Effect (Effect)
import PhotoGalleryLogic (albumChoices, albumPhotos, developedPhoto, isOpen, landscapesOpen, openAlbum)
import PUI (forProperty, mvu, toCase, updated)
import PUI.Web.HTML (shownEach, shown, body, span, staticText, text)
import PUI.Web.MDC2 (divider, drawer, headline2, imageList, imageListItem, imagePane, list, listItem, listOf, overline, topAppBar)
import QualifiedDo.Category as Category

photoGalleryMDC2 :: Effect Unit
photoGalleryMDC2 =
  body $
    topAppBar { title: "Photo Gallery" } $
      ( drawer { title: "Darkroom", subtitle: "photos drawn on the spot" }
          ( RecordToRecord.do
              listOf { selected: isOpen } albumChoices (span (text @"name") # forProperty) # toCase @"albumPicked" _.name # updated (match { albumPicked: const <<< openAlbum })
              divider
              list RecordToRecord.do
                listItem $ staticText "Every photo is an SVG"
                listItem $ staticText "developed from its caption"
                listItem $ staticText "No network involved"
              overline $ staticText "Favorites"
              imageList { columns: 2 } $ RecordToRecord.do
                imageListItem { src: developedPhoto "Dawn Ridge", label: "Dawn Ridge" }
                imageListItem { src: developedPhoto "Half Smile", label: "Half Smile" }
                imageListItem { src: developedPhoto "Orbit Study", label: "Orbit Study" }
                imageListItem { src: developedPhoto "Quiet Lake", label: "Quiet Lake" } )
          ( Category.do
              (headline2 (text @"album")) # shown
              imageList { columns: 3 } $ imagePane # shownEach @"src" albumPhotos )
      ) # mvu landscapesOpen
