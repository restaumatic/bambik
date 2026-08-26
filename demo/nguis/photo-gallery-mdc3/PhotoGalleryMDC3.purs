module PhotoGalleryMDC3 (photoGalleryMDC3) where

import Prelude ((#), ($), (<<<), Unit, const)

import Data.Profunctor.Row.RecordToRecord as RecordToRecord
import Data.Variant (match)
import Effect (Effect)
import PhotoGalleryLogic (albumChoices, albumPhotos, developedPhoto, landscapesOpen, openAlbum)
import PUI (forProperty, mvu, toCase, updated)
import PUI.Web.HTML (shownEach, shown, body, span, staticText, text)
import PUI.Web.MDC3 (divider, drawer, displayMedium, imageList, imageListItem, imagePane, list, listItem, listOf, labelSmall, topAppBar)
import QualifiedDo.Semigroupoid as Pipeline

photoGalleryMDC3 :: Effect Unit
photoGalleryMDC3 =
  body $
    topAppBar { title: "Photo Gallery" } $
      ( drawer { title: "Darkroom", subtitle: "photos drawn on the spot" }
          ( RecordToRecord.do
              listOf { selected: _.current } albumChoices (span (text @"name") # forProperty) # toCase @"albumPicked" _.name # updated (match { albumPicked: const <<< openAlbum })
              divider
              list RecordToRecord.do
                listItem $ staticText "Every photo is an SVG"
                listItem $ staticText "developed from its caption"
                listItem $ staticText "No network involved"
              labelSmall $ staticText "Favorites"
              imageList { columns: 2 } $ RecordToRecord.do
                imageListItem { src: developedPhoto "Dawn Ridge", label: "Dawn Ridge" }
                imageListItem { src: developedPhoto "Half Smile", label: "Half Smile" }
                imageListItem { src: developedPhoto "Orbit Study", label: "Orbit Study" }
                imageListItem { src: developedPhoto "Quiet Lake", label: "Quiet Lake" })
          ( Pipeline.do
              (displayMedium (text @"album")) # shown
              imageList { columns: 3 } $ imagePane # shownEach @"src" albumPhotos )
      ) # mvu landscapesOpen
