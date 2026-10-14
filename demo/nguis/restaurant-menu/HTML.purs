module RestaurantMenu.HTML (restaurantMenu) where

import Prelude ((#), ($), (>>>), Unit, const)

import Effect (Effect)
import PUI (actedAt, foreach, static)
import PUI.Web (cl, staticText, text, textAt, (:=))
import PUI.Web.HTML (a, article, blockquote, body, div, footer, h1, h2, h3, header, hr, li, p, section, span, ul)
import PUI.Web.SVG as SVG
import QualifiedDo.Semigroupoid as Semigroupoid
import RestaurantMenu (dishTags, menuCourses, priceLine)

restaurantMenu :: Effect Unit
restaurantMenu =
  body $ article >>> cl "menu" $ Semigroupoid.do
    header >>> cl "menu-header" $ Semigroupoid.do
      SVG.svg >>> cl "monogram" >>> "viewBox" := "0 0 100 100" >>> "role" := "img" $ Semigroupoid.do
        static (SVG.circle >>> cl "ring" >>> "cx" := "50" >>> "cy" := "50" >>> "r" := "47")
        SVG.text >>> cl "initial" >>> "x" := "50" >>> "y" := "52"
          >>> "text-anchor" := "middle" >>> "dominant-baseline" := "central" $ staticText "Y"
      h1 >>> cl "restaurant-name" $ staticText "Osteria Yoneda"
      p >>> cl "tagline" $ staticText "Cucina componibile — a tasting menu, composed"
    hr $ div >>> cl "courses" $
      ( section >>> cl "course" $ Semigroupoid.do
        h2 $ textAt @"name"
        ul >>> cl "dishes" $
          ( li >>> cl "dish" $ Semigroupoid.do
            div >>> cl "dish-head" $ Semigroupoid.do
              span >>> cl "dish-name" $ textAt @"name"
              static (span >>> cl "dish-dots")
              span >>> cl "dish-price" $ text priceLine
            p >>> cl "dish-desc" $ textAt @"description"
            span >>> cl "tags" $
              ( span >>> cl "tag" $ textAt @"tag" ) # foreach @"tag" @( tag :: String ) dishTags ) # actedAt @"name" @"dishes" ) # foreach @"name"
        @( name :: String
         , dishes :: Array { name :: String, price :: String, description :: String, tags :: Array String }
         ) (const menuCourses)
    blockquote >>> cl "chef-note" $ Semigroupoid.do
      p $ staticText "Every plate is built from a few honest parts that compose into something whole — the same idea that built this page."
      p >>> cl "attribution" $ staticText "— from the kitchen"
    footer >>> cl "menu-footer" $ Semigroupoid.do
      div >>> cl "info" $ Semigroupoid.do
        h3 $ staticText "Hours"
        p $ staticText "Tuesday – Sunday · 17:00 – 23:00"
      div >>> cl "info" $ Semigroupoid.do
        h3 $ staticText "Find us"
        p $ staticText "12 Category Lane · Kraków"
      p >>> cl "colophon" $ Semigroupoid.do
        staticText "A static page composed from HTML oculars with "
        a >>> "href" := "https://github.com/restaumatic/bambik" >>> "target" := "_blank" $ staticText "Bambik"
        staticText " — no Material components, just structure."
