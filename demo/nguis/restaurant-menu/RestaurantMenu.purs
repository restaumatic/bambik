module RestaurantMenu (restaurantMenu) where

import Prelude ((#), ($), (<>), (>>>), Unit, identity)

import Data.Profunctor.Row.RecordToRecord (pempty)
import Data.Profunctor.Row.RecordToRecord as RecordToRecord
import Effect (Effect)
import PUI (forField, forValue, projection, with)
import PUI.HTML (a, article, blockquote, body, cl, div, footer, foreach, h1, h2, h3, header, hr, li, p, section, span, staticText, text, ul, (:=))
import PUI.SVG as SVG

restaurantMenu :: Effect Unit
restaurantMenu =
  body $ ( article >>> cl "menu" $ RecordToRecord.do
    header >>> cl "menu-header" $ RecordToRecord.do
      SVG.svg >>> cl "monogram" >>> "viewBox" := "0 0 100 100" >>> "role" := "img" $ RecordToRecord.do
        SVG.circle >>> cl "ring" >>> "cx" := "50" >>> "cy" := "50" >>> "r" := "47" $ pempty
        SVG.text >>> cl "initial" >>> "x" := "50" >>> "y" := "52"
          >>> "text-anchor" := "middle" >>> "dominant-baseline" := "central" $ staticText "Y"
      h1 >>> cl "restaurant-name" $ staticText "Osteria Yoneda"
      p >>> cl "tagline" $ staticText "Cucina componibile — a tasting menu, composed"
      hr
    div >>> cl "courses" $
      ( section >>> cl "course" $ RecordToRecord.do
          h2 (text # forValue # forField @"name")
          ul >>> cl "dishes" $
            ( li >>> cl "dish" $ RecordToRecord.do
                div >>> cl "dish-head" $ RecordToRecord.do
                  span >>> cl "dish-name" $ text # forValue # forField @"name"
                  span >>> cl "dish-dots" $ pempty
                  span >>> cl "dish-price" $ text # projection ("€" <> _) # forField @"price"
                p >>> cl "dish-desc" $ text # forValue # forField @"description"
                span >>> cl "tags" $
                  ( span >>> cl "tag" $ text # forValue ) # foreach identity # forField @"tags"
            )
            # foreach _.name # forField @"dishes"
      )
      # foreach _.name # forField @"courses"
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

type Dish =
  { name :: String
  , price :: String
  , description :: String
  , tags :: Array String
  }

type Course =
  { name :: String
  , dishes :: Array Dish
  }

courses :: Array Course
courses =
  [ { name: "Antipasti"
    , dishes:
        [ { name: "Burrata di Puglia", price: "14", description: "Heirloom tomato, basil oil, Maldon salt.", tags: [ "vegetarian" ] }
        , { name: "Vitello Tonnato", price: "16", description: "Rose veal, tuna-caper emulsion, fried capers.", tags: [] }
        , { name: "Carciofi alla Romana", price: "13", description: "Braised artichoke, mint, lemon.", tags: [ "vegetarian", "gluten-free" ] }
        ]
    }
  , { name: "Primi"
    , dishes:
        [ { name: "Tagliatelle al Tartufo", price: "24", description: "Fresh egg pasta, black truffle, aged parmigiano.", tags: [ "signature" ] }
        , { name: "Risotto ai Porcini", price: "21", description: "Carnaroli rice, porcini, grana padano.", tags: [ "vegetarian", "gluten-free" ] }
        , { name: "Gnocchi alla Sorrentina", price: "18", description: "Potato gnocchi, San Marzano, fior di latte.", tags: [ "vegetarian" ] }
        ]
    }
  , { name: "Dolci"
    , dishes:
        [ { name: "Tiramisù della Casa", price: "9", description: "Mascarpone, espresso, savoiardi, bitter cocoa.", tags: [ "signature" ] }
        , { name: "Panna Cotta ai Frutti", price: "8", description: "Vanilla cream, forest berries.", tags: [ "gluten-free" ] }
        , { name: "Affogato al Caffè", price: "7", description: "Fior di latte gelato drowned in hot espresso.", tags: [ "vegetarian", "gluten-free" ] }
        ]
    }
  ]
