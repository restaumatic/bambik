module RestaurantMenu (restaurantMenu) where

import Prelude (identity, (#), ($), (>>>), Unit, map)

import Data.Profunctor.Row.RecordToRecord as RecordToRecord
import Effect (Effect)
import PUI (atField, foreach, forProperty, with, static)
import PUI.Web.HTML (a, article, blockquote, body, cl, div, footer, h1, h2, h3, header, hr, li, p, section, span, staticText, text, ul, (:=))
import PUI.Web.SVG as SVG
import RestaurantMenuLogic (courses, dishLines)

restaurantMenu :: Effect Unit
restaurantMenu =
  body $ ( article >>> cl "menu" $ RecordToRecord.do
    header >>> cl "menu-header" $ RecordToRecord.do
      SVG.svg >>> cl "monogram" >>> "viewBox" := "0 0 100 100" >>> "role" := "img" $ RecordToRecord.do
        static (SVG.circle >>> cl "ring" >>> "cx" := "50" >>> "cy" := "50" >>> "r" := "47")
        SVG.text >>> cl "initial" >>> "x" := "50" >>> "y" := "52"
          >>> "text-anchor" := "middle" >>> "dominant-baseline" := "central" $ staticText "Y"
      h1 >>> cl "restaurant-name" $ staticText "Osteria Yoneda"
      p >>> cl "tagline" $ staticText "Cucina componibile — a tasting menu, composed"
      hr
    div >>> cl "courses" $
      ( section >>> cl "course" $ RecordToRecord.do
          h2 (text @"name")
          ul >>> cl "dishes" $
            ( li >>> cl "dish" $ RecordToRecord.do
                div >>> cl "dish-head" $ RecordToRecord.do
                  span >>> cl "dish-name" $ text @"name"
                  static (span >>> cl "dish-dots")
                  span >>> cl "dish-price" $ text @"priceLine"
                p >>> cl "dish-desc" $ text @"description"
                span >>> cl "tags" $
                  ( span >>> cl "tag" $ text @"tag" # forProperty ) # foreach @"tag" (map { tag: _ }) # atField @"tags" ) # foreach @"name" dishLines # atField @"dishes" ) # foreach @"name" identity # atField @"courses"
    blockquote >>> cl "chef-note" $ RecordToRecord.do
      p (staticText "Every plate is built from a few honest parts that compose into something whole — the same idea that built this page.")
      p >>> cl "attribution" $ staticText "— from the kitchen"
    footer >>> cl "menu-footer" $ RecordToRecord.do
      div >>> cl "info" $ RecordToRecord.do
        h3 (staticText "Hours")
        p (staticText "Tuesday – Sunday · 17:00 – 23:00")
      div >>> cl "info" $ RecordToRecord.do
        h3 (staticText "Find us")
        p (staticText "12 Category Lane · Kraków")
      p >>> cl "colophon" $ RecordToRecord.do
        staticText "A static page composed from HTML oculars with "
        a >>> "href" := "https://github.com/restaumatic/bambik" >>> "target" := "_blank" $ staticText "Bambik"
        staticText " — no Material components, just structure."
  ) # with { courses }
