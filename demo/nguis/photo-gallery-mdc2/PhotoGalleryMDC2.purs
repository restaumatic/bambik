module PhotoGalleryMDC2 (photoGalleryMDC2) where

import Prelude (identity, (#), ($), (<<<), Unit, const)

import Data.Profunctor.Row.RecordToRecord as RecordToRecord
import Data.Variant (match)
import Effect (Effect)
import PhotoGalleryLogic (albumChoices, albumPhotos, developedPhoto, landscapesOpen, openAlbum)
import PUI (forProperty, mvu, toCase, updated)
import PUI.Web.HTML (shownEach, shownAs, body, span, staticText, text)
import PUI.Web.MDC2 (divider, drawer, headline2, imageList, imageListItem, imagePane, list, listItem, listOf, overline, topAppBar)
import QualifiedDo.Semigroupoid as Semigroupoid

photoGalleryMDC2 :: Effect Unit
photoGalleryMDC2 =
  body $
    topAppBar { title: "Photo Gallery" } $
      ( drawer { title: "Darkroom", subtitle: "photos drawn on the spot" }
          ( RecordToRecord.do
              listOf { selected: _.current } albumChoices (span (text @"name") # forProperty identity) # toCase @"albumPicked" _.name # updated (match { albumPicked: const <<< openAlbum })
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
                imageListItem { src: developedPhoto "Quiet Lake", label: "Quiet Lake" })
          ( Semigroupoid.do
              shownAs identity (headline2 (text @"album"))
              imageList { columns: 3 } $ shownEach @"src" albumPhotos imagePane )
      ) # mvu landscapesOpen
